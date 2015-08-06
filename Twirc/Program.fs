module Program

open System
open MessageParser

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::oauth::channels ->
        let get r =
            match r with
            | DataLink.Success x -> x
            | DataLink.Failure x -> failwith x

        let uplink = get (DataLink.create "irc.twitch.tv" 6667)
        uplink |> IRC.sendLogin username oauth

        // Must request to receive joins, leaves, and operator status changes
        DataLink.sendLine uplink "CAP REQ :twitch.tv/membership"
        channels |> List.iter (IRC.joinChannel uplink)

        let defaultState : State.State = {
            dataLink = uplink;
            mods = [];
        }

        let rec loop (state:State.State) =
            let read = DataLink.readLine state.dataLink

            if read <> null then
                let newState =
                    match MessageParser.toType read with
                    | Some x ->
                        match x with
                        | MessageParser.Ping content ->
                            DataLink.sendLine state.dataLink (sprintf "PONG %s" content)
                            state
                        | _ ->
                            let msg, newState = State.update x state
                            Display.printMessage msg newState
                            newState
                    | None ->
                        state

                loop newState

        loop defaultState
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0