module Client

open System
open MessageParser

type State = {
    dataLink: DataLink.Link;
    mods: (Channel * User) list;
}

let private cprintf color fmt =
    Printf.kprintf
        (fun s ->
            let old = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write s
            Console.ForegroundColor <- old
        )
        fmt

let private printTime() =
    let time = DateTime.Now.ToString "[hh:mm:ss tt]"
    cprintf ConsoleColor.Green "%s " time

let private printStatusMessage channel user status =
    printTime()
    cprintf ConsoleColor.Cyan "<%s> " channel
    cprintf ConsoleColor.White "%s " user
    cprintf ConsoleColor.Magenta "%s " status
    printfn ""

let processMessage msg state =
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
        state
    | Join (channel, username) ->
        printStatusMessage channel username "joined"
        state
    | Leave (channel, username) ->
        printStatusMessage channel username "left"
        state
    | ModeratorJoin (channel, user) ->
        printTime()
        cprintf ConsoleColor.Cyan "<%s> " channel
        cprintf ConsoleColor.Magenta "Moderator "
        cprintf ConsoleColor.White "%s" user
        cprintf ConsoleColor.Magenta " joined"
        printfn ""

        let newMods = (channel, user)::state.mods
        {state with mods = newMods}
    | ModeratorLeft (channel, user) ->
        printTime()
        cprintf ConsoleColor.Cyan "<%s> " channel
        cprintf ConsoleColor.Magenta "Moderator "
        cprintf ConsoleColor.White "%s" user
        cprintf ConsoleColor.Magenta " left"
        printfn ""

        // TODO: Ensure only moderators from the current channel are removed
        let newMods =
            state.mods
            |> List.filter (fun (_, uname) -> uname <> user)

        {state with mods = newMods}
    | Ping content ->
        DataLink.sendLine state.dataLink (sprintf "PONG %s" content)
        state
    | LoginSuccess username ->
        printTime()
        cprintf ConsoleColor.White "Logged in as "
        cprintf ConsoleColor.Yellow "%s" username
        printfn ""
        state
    | LoginFailed reason ->
        printTime()
        cprintf ConsoleColor.Red "Failed to login: "
        cprintf ConsoleColor.White "%s" reason
        printfn ""
        state