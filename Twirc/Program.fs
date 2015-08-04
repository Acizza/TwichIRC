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
                let sw = System.Diagnostics.Stopwatch.StartNew()
                let r = MessageParser.toType read
                sw.Stop()
                printfn "%f" sw.Elapsed.TotalMilliseconds
                r |> printfn "Type: %A"
                loop()

        loop()
    | _ ->
        printfn "Usage: <username> <oauth> |channels|"

    0