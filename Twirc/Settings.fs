module Settings

open System
open System.IO
open System.Text.RegularExpressions
open Display

type Username = string
type Password = string

type Settings = {
    username: Username;
    password: string;
} with
    static member Zero = {
        username = "";
        password = "";
    }

let read path =
    if not (File.Exists path) then
        File.Create path |> ignore

    let options =
        let str = File.ReadAllText path
        Regex.Matches(str, @"(.+?):(.+)")
        |> Seq.cast<Match>
        |> Seq.toArray

    let readSetting s (name, value) =
        match name with
        | "username" | "user" ->
            {s with username = value}
        | "password" | "pass" ->
            {s with password = value}
        | _ ->
            printTime()
            cprintf ConsoleColor.DarkRed "Unknown setting: "
            cprintf ConsoleColor.DarkGray "%s" name
            printfn ""
            s

    match options.Length with
    | 0 -> Settings.Zero
    | _ ->
        options
        |> Array.map (fun x -> (x.Groups.[1].Value.Trim(), x.Groups.[2].Value.Trim()))
        |> Array.fold readSetting Settings.Zero