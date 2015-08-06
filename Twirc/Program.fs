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

        let defaultState : Client.State = {
            dataLink = uplink;
            mods = [];
        }

        // Temp
        Dispatch.initialState <- defaultState

        let rec readMessages link = async {
            let read = DataLink.readLine link

            if read <> null then
                match MessageParser.toType read with
                | Some x ->
                    Dispatch.fromIRC x
                | None -> ()

                return! readMessages link
        }

        Async.Start (readMessages uplink)

        let rec readConsole() =
            Dispatch.fromConsole (Console.ReadLine())
            readConsole()

        readConsole()
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0