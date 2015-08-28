module Program

open System
open System.IO
open Display
open Message

[<Literal>]
let serverIP = "irc.twitch.tv"

[<Literal>]
let serverPort = 6667

let initialize username password =
    let uplink = DataLink.create serverIP serverPort

    match uplink with
    | DataLink.Success x ->
        x |> Client.sendLogin username password
        DataLink.sendLine x "CAP REQ :twitch.tv/membership"
        Some x
    | DataLink.Failure msg ->
        printErrorStatus "Unable to connect to server" msg
        None

let getInitialState channels uplink =
    let state =
        {Client.State.Zero with dataLink = uplink}

    let newState =
        channels
        |> List.fold Client.joinChannel state

    Some newState

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
    let (>>=) s f = Option.bind f s

    let uploadState s =
        Dispatch.fromState s
        Some s

    initialize username oauth
    >>= getInitialState channels
    >>= uploadState
    >>= startProcessing
    |> ignore

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