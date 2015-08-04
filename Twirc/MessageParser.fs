module MessageParser

type Channel = string
type User = string
type Message = string
type ReplyContent = string

type MessageType =
    | PrivateMessage of Channel * User * Message
    | Join of Channel * User
    | Leave of Channel * User
    | Ping of ReplyContent

let getCode (str:string) =
    if str.StartsWith "PING" then
        Some "PING"
    else
        let split = str.Split ' '
        if split.Length > 1 then Some split.[1] else None

let toType str =
    let getChannel str =
        match str |> String.between "#" [|" "; "\r"|] with
        | Some s -> s
        | None -> "<Unknown>"

    let getUser (str:string) =
        str.[1..str.IndexOf '!'-1]

    match getCode str with
    | Some "PRIVMSG" ->
        let channel = str |> getChannel
        let user = str |> getUser
        let msg = str.[str.IndexOf " :"+2..]

        Some (PrivateMessage (channel, user, msg))
    | Some "JOIN" ->
        let channel = str |> getChannel
        let user = str |> getUser

        Some (Join (channel, user))
    | Some "PART" ->
        let channel = str |> getChannel
        let user = str |> getUser

        Some (Leave (channel, user))
    | Some "PING" ->
        Some (Ping str.["PING ".Length..])
    | _ ->
        None