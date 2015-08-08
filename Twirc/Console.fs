module Console

open System
open Client
open Display

let processMessage (str:string) state =
    let args = str.Split ' ' |> Array.toList

    match args with
    | "join"::channels ->
        channels |> List.iter (IRC.joinChannel state.dataLink)
        state
    | "leave"::channels ->
        channels |> List.iter (IRC.leaveChannel state.dataLink)
        state
    | "send"::channel::messages ->
        let msg = messages |> String.concat " "
        IRC.sendMessage state.dataLink channel msg
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
    | _ ->
        state