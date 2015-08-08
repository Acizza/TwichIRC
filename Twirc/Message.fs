module Message

module private String =
    let between (start:string) (finish:string[]) (str:string) =
        let sIdx = str.IndexOf start

        if sIdx = -1 then
            None
        else
            let newStr = str.[sIdx+start.Length..]

            // Finds the first valid finish index
            let eIdx =
                finish
                |> Array.map newStr.IndexOf
                |> Array.filter (fun x -> x <> -1)
                |> (fun x -> if x.Length > 0 then x.[0] else newStr.Length)

            if eIdx = -1
            then None
            else Some newStr.[..eIdx-1]

type Channel = string
type User = string
type Message = string
type ReplyContent = string

type MessageType =
    | ChatMessage of Channel * User * Message
    | Join of Channel * User
    | Leave of Channel * User
    | ModeratorJoin of Channel * User
    | ModeratorLeft of Channel * User
    | Ping of ReplyContent
    | LoginSuccess of User
    | LoginFailed of Message

let getCode (str:string) =
    if str.StartsWith "PING" then
        Some "PING"
    else
        let split = str.Split ' '
        if split.Length > 1 then Some split.[1] else None

let read str =
    let getChannel str =
        match str |> String.between "#" [|" "; "\r"|] with
        | Some s -> s
        | None -> "<Unknown>"

    let inline getUser (str:string) = str.[1..str.IndexOf '!'-1]
    let inline getChanAndUser str = (str |> getChannel, str |> getUser)

    match getCode str with
    | Some "PRIVMSG" ->
        let channel, user = str |> getChanAndUser
        let msg = str.[str.IndexOf " :"+2..]

        Some (ChatMessage (channel, user, msg))
    | Some "JOIN" ->
        let channel, user = str |> getChanAndUser
        Some (Join (channel, user))
    | Some "PART" ->
        let channel, user = str |> getChanAndUser
        Some (Leave (channel, user))
    | Some "MODE" ->
        let split = str.Split ' '
        let status = split.[3].[0]

        let channel = str |> getChannel
        let user = split.[4]

        if status = '+'
        then Some (ModeratorJoin (channel, user))
        else Some (ModeratorLeft (channel, user))
    | Some "PING" ->
        Some (Ping str.["PING ".Length..])
    | Some "004" ->
        Some (LoginSuccess (str.Split ' ').[2])
    | Some "NOTICE" ->
        Some (LoginFailed str.[str.IndexOf " :"+2..])
    | _ ->
        None