module Twirc.Program

open System
open System.IO
open System.Text
open System.Net.Sockets
open Twirc.IRC

let rec processInput (input:string) (writer:StreamWriter) =
    let args = input.Split ' ' |> Array.toList

    match args with
    | "join"::channels ->
        channels |> List.iter (fun s -> join s writer)
    | "leave"::channels ->
        channels |> List.iter (fun s -> leave s writer)
    | "send"::channel::msg ->
        let fullMsg = msg |> String.concat " "
        writer |> sendMessage channel fullMsg
    | _ ->
        println "Unknown command: %s" input
        ()

    processInput (Console.ReadLine()) writer

[<EntryPoint>]
let main args =
    match args |> Array.toList with
    | username::password::channels ->
        use client = new TcpClient("irc.twitch.tv", 6667)
        use reader = new StreamReader(client.GetStream(), Encoding.UTF8)
        use writer = new StreamWriter(client.GetStream(), Encoding.UTF8)

        writer.NewLine <- "\r\n"

        let state = {
            stream = writer;
            username = username;
            mods = [];
        }

        Async.Start (processMessages reader state)

        writer |> login username password

        channels
        |> List.iter (fun c -> join c writer)

        writer |> processInput (Console.ReadLine())
    | _ ->
        printfn "Usage: <username> <oauth>"

    0