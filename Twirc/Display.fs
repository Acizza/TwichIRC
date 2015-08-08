module Display

open System

let cprintf color fmt =
    Printf.kprintf
        (fun s ->
            let old = Console.ForegroundColor
            Console.ForegroundColor <- color
            Console.Write s
            Console.ForegroundColor <- old
        )
        fmt

let inline printTime() =
    let time = DateTime.Now.ToString "[hh:mm:ss tt]"
    cprintf ConsoleColor.DarkGray "%s " time

let inline printChannel channel =
    cprintf ConsoleColor.DarkCyan "<%s> " channel

let inline printTimeAndChannel channel =
    printTime()
    printChannel channel

let printStatusMessage channel user status =
    printTimeAndChannel channel
    cprintf ConsoleColor.DarkYellow "%s " user
    cprintf ConsoleColor.DarkMagenta "%s " status
    printfn ""