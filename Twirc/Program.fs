open System
open System.IO
open System.Text
open System.Net.Sockets

let cprintf color fmt =
    Printf.kprintf
        (fun s ->
            //let old = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write s
            //Console.ForegroundColor <- old
        )
        fmt

let time() =
    DateTime.Now.ToString("[hh:mm:ss tt]")

let printTime() =
    cprintf ConsoleColor.Green "%s" (time())

let printStatusMessage channel user status =
    printTime()
    cprintf ConsoleColor.Cyan " <%s> " channel
    cprintf ConsoleColor.Yellow "%s " user
    cprintf ConsoleColor.White "%s " status
    printfn ""

type PipeStatus<'a> =
    | Continue of 'a
    | Halt

let processCritical (line:string) =
    let code =
        let split = line.Split ' '
        if split.Length > 0 then split.[1] else ""

    match code with
    | "004" ->
        printfn "Logged in"
        Halt
    | "NOTICE" ->
        printfn "Failed to login"
        Halt
    | _ ->
        Continue (code,line)

let processMessage (code,line:string) =
    let getIndexOrEnd (str:string) (list:string[]) =
        let res = list
                |> Array.map (fun s -> str.IndexOf s)
                |> Array.filter (fun x -> x <> -1)

        if res.Length > 0 then res.[0] else str.Length

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
        if username = "twitchnotify" then
            let user =
                let msg = getMessage()
                msg.Substring(0, msg.IndexOf " ")

            printStatusMessage (getChannel()) user (getMessage().Substring(user.Length+1))
        else
            printTime()
            cprintf ConsoleColor.Cyan " <%s> " (getChannel())
            cprintf ConsoleColor.Yellow "%s" username
            cprintf ConsoleColor.White ": %s" (getMessage())
            printfn ""
    | "JOIN" ->
        printStatusMessage (getChannel()) username "joined"
    | "PART" ->
        printStatusMessage (getChannel()) username "left"
    | _ ->
        ()

    Halt

let processLine line =
    let ifContinueThen func status =
        match status with
        | Continue s -> func s
        | Halt -> Halt

    line
    |> processCritical
    |> ifContinueThen processMessage

let rec processMessages (reader:StreamReader) (writer:StreamWriter) =
    let line = reader.ReadLine()

    match reader.EndOfStream with
    | false ->
        if line.Length > 0 then
            if line.StartsWith "PING " then
                writer.WriteLine ("PONG " + line.Substring("PING ".Length))
                writer.Flush()
            else
                processLine line |> ignore

        processMessages reader writer
    | true ->
        ()

let login nick pass (writer:StreamWriter) =
    writer.WriteLine()
    writer.WriteLine(sprintf "USER %s 0 * :%s" nick nick)
    writer.WriteLine(sprintf "PASS %s" pass)
    writer.WriteLine(sprintf "NICK %s" nick)
    writer.Flush()

let join channel (writer:StreamWriter) =
    writer.WriteLine(sprintf "JOIN #%s" channel)
    writer.Flush()

[<EntryPoint>]
let main args = 
    use client = new TcpClient("irc.twitch.tv", 6667)
    use reader = new StreamReader(client.GetStream(), Encoding.UTF8)
    use writer = new StreamWriter(client.GetStream(), Encoding.UTF8)

    writer.NewLine <- "\r\n"

    // Replace the empty strings with your user credentials (username, oauth)
    writer |> login "" ""
    // Replace the empty string with the channel you want to join
    writer |> join ""

    processMessages reader writer
    0