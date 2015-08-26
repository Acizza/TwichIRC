module Command

open System
open Client
open Display

//* Start of command list *\\

let private joinCommand state =
    List.fold Client.joinChannel state

let private leaveCommand state =
    List.fold Client.leaveChannel state

let private sendCommand state =
    function
    | channel::messages ->
        let msg = messages |> String.concat " "
        Client.sendMessage state channel msg
        state
    | _ -> state

let private modsCommand state =
    function
    | channel::_ ->
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
    | _ -> state

let private channelsCommand state _ =
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

let private leaveallCommand state _ =
    state.channels
    |> List.fold Client.leaveChannel state

//* End of command list *\\



type Name = string
type UsageDesc = string
type ArgCount = int
type Arguments = string list
type Callback = Client.State -> Arguments -> Client.State

let commands : (Name * UsageDesc * ArgCount * Callback) array = [|
    ("join", "<channels>", 1, joinCommand);
    ("leave", "<channels>", 1, leaveCommand);
    ("send", "<channel> <message>", 2, sendCommand);
    ("mods", "<channel>", 1, modsCommand);
    ("channels", "", 0, channelsCommand);
    ("leaveall", "", 0, leaveallCommand);
|]



let find name =
    commands
    |> Array.tryFind (fun (n,_,_,_) -> n = name)

let (|Command|) input =
    match input with
    | name::args ->
        match find name with
        | Some x -> Some (x, args)
        | None -> None
    | _ ->
        None

let execute (str:string) (state:Client.State) =
    let input = str.Split ' ' |> Array.toList

    match input with
    | Command (Some (c, args)) ->
        let (_,usageDesc,minArgs,func) = c

        match args.Length with
        | l when l < minArgs ->
            printErrorStatus
                (sprintf "Received %d/%d arguments" l minArgs)
                usageDesc

            state
        | _ -> func state args
    | "commands"::_ -> // TODO: Look into better way of handling commands that reference the command list
        printTime()
        cprintf ConsoleColor.DarkYellow "%d" commands.Length
        cprintf ConsoleColor.DarkMagenta " Commands:%s" Environment.NewLine

        let printCommand c =
            printTime()

            let (name,usageDesc,_,_) = c
            cprintf ConsoleColor.DarkYellow "%s" name
            cprintf ConsoleColor.DarkGray " %s" usageDesc

            printfn ""

        commands |> Array.iter printCommand
        state
    | [] | [""] ->
        printError "Input must not be empty"
        state
    | name::_ ->
        printErrorStatus "Command not found" name
        state