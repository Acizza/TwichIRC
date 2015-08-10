module Program

open System
open Message

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::oauth::channels ->
        let get r =
            match r with
            | DataLink.Success x -> x
            | DataLink.Failure x -> failwith x

        let uplink = get (DataLink.create "irc.twitch.tv" 6667)
        uplink |> Client.sendLogin username oauth

        let defaultState : Client.State = {
            dataLink = uplink;
            channels = [];
            mods = [];
        }

        // Must request to receive joins, leaves, and operator status changes
        DataLink.sendLine uplink "CAP REQ :twitch.tv/membership"

        let initialState =
            channels
            |> List.fold Client.joinChannel defaultState

        let rec readMessages link = async {
            let str = DataLink.readLine link

            if str <> null then
                match Message.read str with
                | Some x -> Dispatch.fromIRC x
                | None -> ()

                return! readMessages link
        }

        Dispatch.fromState initialState
        Async.Start (readMessages uplink)

        let rec readConsole() =
            Dispatch.fromConsole (Console.ReadLine())
            readConsole()

        readConsole()
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0