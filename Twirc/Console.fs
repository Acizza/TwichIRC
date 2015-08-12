module Console

open System
open Client
open Display

let processMessage (str:string) (state:Client.State) =
    let args = str.Split ' ' |> Array.toList

    match args with
    | "join"::channels ->
        channels
        |> List.fold Client.joinChannel state
    | "leave"::channels ->
        channels
        |> List.fold Client.leaveChannel state
    | "send"::channel::messages ->
        let msg = messages |> String.concat " "
        Client.sendMessage state channel msg
        state
    | "mods"::channel::_ ->
        let mods =
            state.mods
            |> List.filter (fun (chan,_) -> chan = channel)

        printTimeAndChannel channel
        cprintf ConsoleColor.DarkYellow "%d" mods.Length
        cprintf ConsoleColor.DarkMagenta " Moderators:%s" Environment.NewLine

        let printMod (_,name) =
            printTimeAndChannel channel
            cprintf ConsoleColor.DarkYellow "%s" name
            printfn ""

        mods |> List.iter printMod
        state
    | "channels"::_ ->
        let channels = state.channels

        printTime()
        cprintf ConsoleColor.DarkYellow "%d" channels.Length
        cprintf ConsoleColor.DarkMagenta " Channels:%s" Environment.NewLine

        let printChannel channel =
            printTime()
            cprintf ConsoleColor.DarkYellow "%s" channel
            printfn ""

        channels |> List.iter printChannel
        state
    | "leaveall"::_ ->
        state.channels
        |> List.fold Client.leaveChannel state
    | [] | ""::_ ->
        printError "Input must not be empty"
        state
    | command::_ ->
        printErrorStatus "Invalid command" command
        state