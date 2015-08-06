module Display

open System
open MessageParser
open State

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

let printMessage msg state =
    let isModerator channel user =
        state.mods
        |> List.exists (fun (chan, uname) -> chan = channel && uname = user)    

    match msg with
    | ChatMessage (channel, user, msg) ->
        printTime()
        cprintf ConsoleColor.Cyan "<%s> " channel

        if user = channel then
            cprintf ConsoleColor.Gray "[B] "
        elif user |> isModerator channel then
            cprintf ConsoleColor.Gray "[M] "

        cprintf ConsoleColor.Yellow "%s" user
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
    | _ -> ()