module Twirc.Program

open System
open System.IO
open System.Text
open System.Net.Sockets
open Twirc.IRC

let processInput (input:string) state =
    let args = input.Split ' ' |> Array.toList
    let writer = state.writer

    match args with
    | "join"::channels ->
        channels |> List.iter (fun s -> join s writer)
        state
    | "leave"::channels ->
        channels |> List.iter (fun s -> leave s writer)
        state
    | "send"::channel::msg ->
        let fullMsg = msg |> String.concat " "
        writer |> sendMessage channel fullMsg
        state
    | _ ->
        println "Unknown command: %s" input
        state

type Message = IRC of string | Console of string

let rec readMessages (agent:MailboxProcessor<Message>) (reader:StreamReader) = async {
    let line = reader.ReadLine()

    if line <> null then
        agent.Post (IRC line)
        return! readMessages agent reader
}

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::password::channels ->
        use client = new TcpClient("irc.twitch.tv", 6667)
        use reader = new StreamReader(client.GetStream(), Encoding.UTF8)
        use writer = new StreamWriter(client.GetStream(), Encoding.UTF8)

        writer.NewLine <- "\r\n"

        let state = {
            reader = reader;
            writer = writer;
            username = username;
            mods = [];
        }

        let agent = MailboxProcessor.Start (fun inbox ->
            let rec loop state = async {
                let! msg = inbox.Receive()

                let next =
                    match msg with
                    | IRC line ->
                        processMessage line state
                    | Console input ->
                        processInput input state

                return! loop next
            }

            loop state
        )

        Async.Start (readMessages agent reader)

        writer |> login username password

        channels
        |> List.iter (fun c -> join c writer)

        let rec processConsole() =
            agent.Post (Console (Console.ReadLine()))
            processConsole()

        processConsole()
    | _ ->
        printfn "Usage: <username> <oauth>"

    0