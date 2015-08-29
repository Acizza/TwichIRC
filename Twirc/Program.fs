module Program

open System
open System.IO
open Result
open Message
open Display

[<Literal>]
let serverIP = "irc.twitch.tv"

[<Literal>]
let serverPort = 6667

let createUplink() = DataLink.create serverIP serverPort

let login username password (client:Client.State) =
    client.dataLink |> Client.sendLogin username password
    DataLink.sendLine client.dataLink "CAP REQ :twitch.tv/membership"

    Success client

let joinChannels channels (client:Client.State) =
    let newState =
        channels
        |> List.fold Client.joinChannel client

    Success newState

let startProcessing (state:Client.State) =
    let rec processServer link = async {
        let msg = link |> DataLink.readLine

        match msg with
        | null -> ()
        | _ ->
            match Message.readType msg with
            | Some x -> Dispatch.fromIRC x
            | None -> ()
            return! processServer link
    }

    let rec processConsole() =
        Dispatch.fromCommand (Console.ReadLine())
        processConsole()

    Async.Start (processServer state.dataLink)
    processConsole()

let run username oauth channels =
    let (>>=) s f =
        match s with
        | Success x -> f x
        | Failure msg -> Failure msg

    let createClient uplink =
        Success (Client.createFromUplink uplink)

    let uploadClient s =
        Dispatch.fromClient s
        Success s

    let printErrors =
        function
        | Success _ -> ()
        | Failure msg ->
            printErrorStatus "Error initializing" msg

    createUplink()
    >>= createClient
    >>= login username oauth
    >>= joinChannels channels
    >>= uploadClient
    >>= startProcessing
    |> printErrors

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::oauth::channels ->
        run username oauth channels
    | channels ->
        let (|ValidFile|) (s:Settings.Settings option) =
            match s with
            | Some x
                when x.username <> "" && x.password <> "" ->
                    Some x
            | _ -> None

        match Settings.readDefault() with
        | ValidFile (Some x) -> run x.username x.password (channels |> List.append x.joinChannels)
        | _ -> printfn "Usage: <username> <oauth> |channels|"

    0