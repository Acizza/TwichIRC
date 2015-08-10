module Client

open System
open Message
open Display

type State = {
    dataLink: DataLink.Link;
    channels: Channel list;
    mods: (Channel * User) list;
}
with
    static member Zero = {
        dataLink = {client = null; reader = null; writer = null};
        channels = [];
        mods = [];
    }

let sendLogin nick oauth uplink =
    let send = DataLink.queueLine uplink
    send ""
    send (sprintf "USER %s 0 * :%s" nick nick)
    send (sprintf "PASS %s" oauth)
    send (sprintf "NICK %s" nick)
    DataLink.flush uplink

let joinChannel state channel =
    DataLink.sendLine state.dataLink (sprintf "JOIN #%s" channel)
    {state with channels = channel::state.channels}

let leaveChannel state channel =
    DataLink.sendLine state.dataLink (sprintf "PART #%s" channel)

    let newChannels =
        state.channels
        |> List.filter ((<>) channel)

    let newMods =
        state.mods
        |> List.filter (fun (chan,_) -> chan <> channel)

    {state with
        channels = newChannels;
        mods = newMods;
    }

let inline sendMessage state channel msg =
    DataLink.sendLine state.dataLink (sprintf "PRIVMSG #%s :%s" channel msg)

let processMessage msg state =
    let isModerator channel user =
        state.mods
        |> List.exists (fun (chan, uname) -> chan = channel && uname = user)    

    match msg with
    | ChatMessage (channel, user, msg) ->
        printTimeAndChannel channel

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
        printTimeAndChannel channel
        cprintf ConsoleColor.DarkMagenta "Moderator "
        cprintf ConsoleColor.DarkYellow "%s" user
        cprintf ConsoleColor.DarkMagenta " joined"
        printfn ""

        let newMods = (channel, user)::state.mods
        {state with mods = newMods}
    | ModeratorLeft (channel, user) ->
        printTimeAndChannel channel
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