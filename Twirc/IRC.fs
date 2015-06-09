module Twirc.IRC

open System
open System.IO

let cprintf color fmt =
    Printf.kprintf
        (fun s ->
            let old = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write s
            Console.ForegroundColor <- old
        )
        fmt

let curTime() =
    DateTime.Now.ToString("[hh:mm:ss tt]")

let printTime() =
    cprintf ConsoleColor.Green "%s" (curTime())

let println fmt =
    printTime()
    printf " "
    printfn fmt

let printStatusMessage channel user status =
    printTime()
    cprintf ConsoleColor.Cyan " <%s> " channel
    cprintf ConsoleColor.White "%s " user
    cprintf ConsoleColor.Magenta "%s " status
    printfn ""

type PipeStatus<'a,'b> =
    | Continue of 'a
    | Result of 'b
    | Halt

type Channel = string
type Username = string

type State = {
    stream: StreamWriter;
    mods: (Channel * Username) list;
}

/// Processes messages that indicate if a login was successful or not, along with channel moderators.
let processImportant (code,line:string,state) =
    match code with
    | "004" ->
        println "Logged in"
        Halt
    | "NOTICE" ->
        println "Failed to login"
        Halt
    | "MODE" ->
        let split = (line.Split ' ')
        let channel = split.[2]
        let mode = split.[3].[0]
        let username = split.[4]

        if mode = '+' then
            Result {state with mods = (channel.[1..],username)::state.mods}
        else
            let newMods =
                state.mods
                |> List.filter (fun (chan,uname) -> uname <> username && chan <> channel)

            Result {state with mods = newMods}
    | _ ->
        Continue (code,line,state)

/// Processes messages that are printed. Ex: PRIVMSG, JOIN, PART.
let processNormal (code,line:string,state) =
    /// Returns the index of the first found character in the list, or the end of the string otherwise.
    let getIndexOrEnd (str:string) (list:string[]) =
        list
        |> Array.map (fun s -> str.IndexOf s)
        |> Array.filter (fun x -> x <> -1)
        |> (fun x -> if x.Length > 0 then x.[0] else str.Length)

    let getMessage() =
        line.Substring(line.IndexOf " :"+2)

    let getChannel() =
        let start = line.Substring (line.IndexOf "#"+1)
        let eIdx = [|" "; "\r"|] |> getIndexOrEnd start
        start.Substring(0, eIdx)

    let username =
        let eIdx = line.IndexOf '!'

        if eIdx <> -1 then
            let start = line.Substring 1
            start.Substring(0, eIdx-1)
        else
            ""

    match code with
    | "PRIVMSG" ->
        let message = getMessage()
        let channel = getChannel()

        if username = "twitchnotify" then
            let user = message.Substring(0, message.IndexOf " ")
            printStatusMessage channel user (message.Substring(user.Length+1))
        else
            printTime()
            cprintf ConsoleColor.Cyan " <%s> " channel

            if state.mods |> List.exists (fun (chan,uname) -> uname = username && chan = channel) then
                cprintf ConsoleColor.Gray "[M] "

            cprintf ConsoleColor.Yellow "%s" username
            cprintf ConsoleColor.White ": %s" message
            printfn ""
    | "JOIN" ->
        printStatusMessage (getChannel()) username "joined"
    | "PART" ->
        printStatusMessage (getChannel()) username "left"
    | _ ->
        ()

    Result state

let processMessage (line:string) state =
    if line.StartsWith "PING " then
        let writer = state.stream
        writer.WriteLine ("PONG " + line.Substring("PING ".Length))
        writer.Flush()
        state
    else
        let ifContinueThen func status =
            match status with
            | Continue s -> func s
            | Result x -> Result x
            | Halt -> Halt

        let getResultOr def status =
            match status with
            | Result x | Continue x -> x
            | Halt -> def

        let code =
            let split = line.Split ' '
            if split.Length > 0 then split.[1] else ""

        (code,line,state)
        |> processImportant
        |> ifContinueThen processNormal
        |> getResultOr state

let rec processMessages (reader:StreamReader) state = async {
    let line = reader.ReadLine()

    if line <> null then
        let newState = processMessage line state
        return! processMessages reader newState
    else
        ()
}

let login nick pass (writer:StreamWriter) =
    writer.WriteLine()
    writer.WriteLine(sprintf "USER %s 0 * :%s" nick nick)
    writer.WriteLine(sprintf "PASS %s" pass)
    writer.WriteLine(sprintf "NICK %s" nick)
    writer.Flush()

let join channel (writer:StreamWriter) =
    writer.WriteLine(sprintf "JOIN #%s" channel)
    writer.Flush()

let leave channel (writer:StreamWriter) =
    writer.WriteLine(sprintf "PART #%s" channel)
    writer.Flush()

let sendMessage channel msg (writer:StreamWriter) =
    writer.WriteLine(sprintf "PRIVMSG #%s :%s" channel msg)
    writer.Flush()