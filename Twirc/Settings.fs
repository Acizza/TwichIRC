module Settings

open System
open System.IO
open System.Text.RegularExpressions
open Display

type Username = string
type Password = string
type Channel = string

type Settings = {
    username: Username;
    password: Password;
    joinChannels: Channel list;
} with
    static member Zero = {
        username = "";
        password = "";
        joinChannels = [];
    }

let read path =
    match File.Exists path with
    | true ->
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
            | "channels" ->
                let c = value.Split ' ' |> Array.toList
                {s with joinChannels = c}
            | _ ->
                printErrorStatus "Unknown setting" name
                s

        let vIndex (i:int) (m:Match) = m.Groups.[i].Value.Trim()

        let settings =
            match options.Length with
            | 0 -> Settings.Zero
            | _ ->
                options
                |> Array.map (fun x -> (x |> vIndex 1, x |> vIndex 2))
                |> Array.fold readSetting Settings.Zero

        Some settings
    | false ->
        None