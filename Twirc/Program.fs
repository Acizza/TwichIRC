open System
open System.IO
open System.Text
open System.Net.Sockets

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

type PipeStatus<'a> =
    | Continue of 'a
    | Halt

let processCritical (line:string) =
    let code =
        let split = line.Split ' '
        if split.Length > 0 then split.[1] else ""

    match code with
    | "004" ->
        println "Logged in"
        Halt
    | "NOTICE" ->
        println "Failed to login"
        Halt
    | _ ->
        Continue (code,line)

let processMessage (code,line:string) =
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

let rec processMessages (reader:StreamReader) (writer:StreamWriter) = async {
    let line = reader.ReadLine()

    if line <> null then
        if line.Length > 0 then
            if line.StartsWith "PING " then
                writer.WriteLine ("PONG " + line.Substring("PING ".Length))
                writer.Flush()
            else
                processLine line |> ignore

        return! processMessages reader writer
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

let rec processInput (input:string) (writer:StreamWriter) =
    let args = input.Split ' ' |> Array.toList

    match args with
    | "join"::channels ->
        channels |> List.iter (fun s -> join s writer)
    | "leave"::channels ->
        channels |> List.iter (fun s -> leave s writer)
    | "send"::channel::msg ->
        let fullMsg = msg |> String.concat " "
        writer |> sendMessage channel fullMsg
    | _ ->
        println "Unknown command: %s" input
        ()

    processInput (Console.ReadLine()) writer

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::password::channels ->
        use client = new TcpClient("irc.twitch.tv", 6667)
        use reader = new StreamReader(client.GetStream(), Encoding.UTF8)
        use writer = new StreamWriter(client.GetStream(), Encoding.UTF8)

        writer.NewLine <- "\r\n"
        Async.Start (processMessages reader writer)

        writer |> login username password

        channels
        |> List.iter (fun c -> join c writer)

        writer |> processInput (Console.ReadLine())
    | _ ->
        printfn "Usage: <username> <oauth>"

    0