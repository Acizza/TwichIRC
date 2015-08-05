module Program

open System
open MessageParser
open Display

type State = {
    dataLink: DataLink.Link;
    mods: (Channel * User) list;
}

let processMessage msg state =
    let isModerator channel user =
        state.mods
        |> List.exists (fun (chan, uname) -> chan = channel && uname = user)

    match msg with
    | ChatMessage (channel, user, msg) ->
        printTime()
        cprintf ConsoleColor.Cyan "<%s> " channel

        if user |> isModerator channel then
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
    | ModeratorJoin (channel, username) ->
        let newMods = (channel, username)::state.mods
        {state with mods = newMods}
    | ModeratorLeft (channel, username) ->
        // TODO: Ensure only moderators from the current channel are removed
        let newMods =
            state.mods
            |> List.filter (fun (_, uname) -> uname <> username)

        {state with mods = newMods}
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
    | _ ->
        state

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::oauth::channels ->
        let get r =
            match r with
            | Result.Success x -> x
            | Result.Failure x -> failwith x

        let link = get (DataLink.create "irc.twitch.tv" 6667)
        let user = link |> User.createAndLogin username oauth

        // Must request to receive joins, leaves, and operator status changes
        DataLink.sendLine link "CAP REQ :twitch.tv/membership"
        channels |> List.iter (User.joinChannel user)

        let defaultState = {
            dataLink = link;
            mods = [];
        }

        let rec loop state =
            let read = DataLink.readLine state.dataLink

            if read <> null then
                let newState =
                    match MessageParser.toType read with
                    | Some x ->
                        match x with
                        | MessageParser.Ping content -> DataLink.sendLine link (sprintf "PONG %s" content); state
                        | _ -> processMessage x state
                    | None ->
                        state

                loop newState

        loop defaultState
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0