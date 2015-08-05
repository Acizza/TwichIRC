module Program

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

        channels |> List.iter (User.joinChannel user)

        let rec loop() =
            let read = DataLink.readLine link

            if read <> null then
                match MessageParser.toType read with
                | Some x ->
                    match x with
                    | MessageParser.Ping content -> DataLink.sendLine link (sprintf "PONG %s" content); printfn "Sending PONG"
                    | _ -> Display.printMessage x
                | None ->
                    ()

                loop()

        loop()
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0