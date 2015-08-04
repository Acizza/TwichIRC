module Result

type T<'a, 'b> =
    | Success of 'a
    | Failure of 'b

let bind func input =
    match input with
    | Success x -> func x
    | Failure msg -> Failure msg