module Client

open System
open Message

type State = {
    dataLink: DataLink.Link;
    mods: (Channel * User) list;
}
with
    static member Zero = {
        dataLink = {client = null; reader = null; writer = null};
        mods = [];
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
    cprintf ConsoleColor.DarkGray "%s " time

let private printStatusMessage channel user status =
    printTime()
    cprintf ConsoleColor.DarkCyan "<%s> " channel
    cprintf ConsoleColor.DarkYellow "%s " user
    cprintf ConsoleColor.DarkMagenta "%s " status
    printfn ""

let processMessage msg state =
    let isModerator channel user =
        state.mods
        |> List.exists (fun (chan, uname) -> chan = channel && uname = user)    

    match msg with
    | ChatMessage (channel, user, msg) ->
        printTime()
        cprintf ConsoleColor.DarkCyan "<%s> " channel

        if user = channel then
            cprintf ConsoleColor.DarkGray "[B] "
        elif user |> isModerator channel then
            cprintf ConsoleColor.DarkGray "[M] "

        cprintf ConsoleColor.DarkGreen "%s" user
        cprintf ConsoleColor.Gray ": %s" msg
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
        cprintf ConsoleColor.DarkCyan "<%s> " channel
        cprintf ConsoleColor.DarkMagenta "Moderator "
        cprintf ConsoleColor.DarkYellow "%s" user
        cprintf ConsoleColor.DarkMagenta " joined"
        printfn ""

        let newMods = (channel, user)::state.mods
        {state with mods = newMods}
    | ModeratorLeft (channel, user) ->
        printTime()
        cprintf ConsoleColor.DarkCyan "<%s> " channel
        cprintf ConsoleColor.DarkMagenta "Moderator "
        cprintf ConsoleColor.DarkYellow "%s" user
        cprintf ConsoleColor.DarkMagenta " left"
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
        cprintf ConsoleColor.Gray "Logged in as "
        cprintf ConsoleColor.DarkYellow "%s" username
        printfn ""
        state
    | LoginFailed reason ->
        printTime()
        cprintf ConsoleColor.DarkRed "Failed to login: "
        cprintf ConsoleColor.Gray "%s" reason
        printfn ""
        state