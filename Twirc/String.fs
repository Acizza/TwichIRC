module String

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