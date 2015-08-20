module Display

open System

let private parseColorScheme map =
    let toColor s =
        Enum.Parse(typeof<ConsoleColor>, s)
        :?> ConsoleColor

    let parseColor s (key:string) value =
        match key with
        | _ when key.StartsWith "color" ->
            try
                s |> Map.add
                    (key.["color".Length..] |> toColor)
                    (value |> toColor)
            with
            | ex ->
                printfn "Unknown color: %s" key
                s
        | _ ->
            s

    map |> Map.fold parseColor Map.empty

let getSchemeColor =
    let scheme =
        match Settings.readDefault() with
        | Some x ->
            parseColorScheme x.other
        | None -> Map.empty;

    // Caching color scheme binding above by returning a new function.
    // Could also memoize the scheme, but that's a decent amount slower.
    fun color ->
        if scheme.ContainsKey color
        then scheme.[color]
        else color

let cprintf color fmt =
    Printf.kprintf
        (fun s ->
            let old = Console.ForegroundColor
            Console.ForegroundColor <- getSchemeColor color
            Console.Write s
            Console.ForegroundColor <- old
        )
        fmt

let printTime() =
    let time = DateTime.Now.ToString "[hh:mm:ss tt]"
    cprintf ConsoleColor.DarkGray "%s " time

let printError msg =
    printTime()
    cprintf ConsoleColor.DarkRed "%s" msg
    printfn ""

let printErrorStatus header msg =
    printTime()
    cprintf ConsoleColor.DarkRed "%s: " header
    cprintf ConsoleColor.DarkGray "%s" msg
    printfn ""

let printChannel channel =
    cprintf ConsoleColor.DarkCyan "<%s> " channel

let printTimeAndChannel channel =
    printTime()
    printChannel channel

let printStatusMessage channel user status =
    printTimeAndChannel channel
    cprintf ConsoleColor.DarkYellow "%s " user
    cprintf ConsoleColor.DarkMagenta "%s " status
    printfn ""