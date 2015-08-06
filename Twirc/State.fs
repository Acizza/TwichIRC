module State

open MessageParser

type State = {
    dataLink: DataLink.Link;
    mods: (Channel * User) list;
}

let update msg state =
    match msg with
    | ModeratorJoin (channel, user) ->
        let newMods = (channel, user)::state.mods
        (msg, {state with mods = newMods})
    | ModeratorLeft (channel, username) ->
        // TODO: Ensure only moderators from the current channel are removed
        let newMods =
            state.mods
            |> List.filter (fun (_, uname) -> uname <> username)

        (msg, {state with mods = newMods})
    | _ ->
        (msg, state)