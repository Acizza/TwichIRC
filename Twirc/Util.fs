module Util

open System.Collections.Generic

let memoize f =
    let dict = new Dictionary<_, _>()

    fun n ->
        match dict.TryGetValue n with
        | true, v -> v
        | _ ->
            let temp = f n
            dict.Add(n, temp)
            temp