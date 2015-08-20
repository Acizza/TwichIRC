module Settings

open System
open System.IO
open System.Text.RegularExpressions

type Username = string
type Password = string
type Channel = string

type Name = string
type Value = string

type Settings = {
    username: Username;
    password: Password;
    joinChannels: Channel list;
    other: Map<Name, Value>;
} with
    static member Zero = {
        username = "";
        password = "";
        joinChannels = [];
        other = Map.empty;
    }

[<Literal>]
let defaultFile = "settings.cfg"

let forceRead path =
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
                {s with other = s.other |> Map.add name value}

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

/// Returns cached version of settings file, or tries to read it
let read = Util.memoize forceRead
/// Returns cached version of settings file, or tries to read it
let readDefault() = defaultFile |> Util.memoize forceRead