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
        uplink |> IRC.sendLogin username oauth

        // Must request to receive joins, leaves, and operator status changes
        DataLink.sendLine uplink "CAP REQ :twitch.tv/membership"
        channels |> List.iter (IRC.joinChannel uplink)

        let rec readMessages link = async {
            let str = DataLink.readLine link

            if str <> null then
                match Message.read str with
                | Some x -> Dispatch.fromIRC x
                | None -> ()

                return! readMessages link
        }

        let defaultState : Client.State = {
            dataLink = uplink;
            mods = [];
        }

        Dispatch.fromState defaultState
        Async.Start (readMessages uplink)

        let rec readConsole() =
            Dispatch.fromConsole (Console.ReadLine())
            readConsole()

        readConsole()
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0