module Display

open System
open MessageParser

let cprintf color fmt =
    Printf.kprintf
        (fun s ->
            let old = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write s
            Console.ForegroundColor <- old
        )
        fmt

let printTime() =
    let time = DateTime.Now.ToString "[hh:mm:ss tt]"
    cprintf ConsoleColor.Green "%s " time

let printStatusMessage channel user status =
    printTime()
    cprintf ConsoleColor.Cyan "<%s> " channel
    cprintf ConsoleColor.White "%s " user
    cprintf ConsoleColor.Magenta "%s " status
    printfn ""

let printMessage msg =
    match msg with
    | ChatMessage (channel, username, msg) ->
        printTime()
        cprintf ConsoleColor.Cyan "<%s> " channel
        cprintf ConsoleColor.Yellow "%s" username
        cprintf ConsoleColor.White ": %s" msg
        printfn ""
    | Join (channel, username) ->
        printStatusMessage channel username "joined"
    | Leave (channel, username) ->
        printStatusMessage channel username "left"
    | LoginSuccess username ->
        printTime()
        cprintf ConsoleColor.White "Logged in as "
        cprintf ConsoleColor.Yellow "%s" username
        printfn ""
    | LoginFailed reason ->
        printTime()
        cprintf ConsoleColor.Red "Failed to login: "
        cprintf ConsoleColor.White "%s" reason
        printfn ""
    | _ ->
        ()