module Console

open Client

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
    | _ ->
        state