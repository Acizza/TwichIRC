module Dispatch

type Type =
    | IRC of MessageParser.MessageType
    | Console of string

// Temp
let mutable initialState : Client.State = {
    dataLink = {client = null; reader = null; writer = null};
    mods = [];
}

let private agent = MailboxProcessor.Start (fun inbox ->
    let rec loop state = async {
        let! msg = inbox.Receive()

        let nextState =
            match msg with
            | IRC msg -> Client.processMessage msg state
            | Console str -> Console.processMessage str state

        return! loop nextState
    }
    
    loop initialState
)

let fromType t = agent.Post t
let fromIRC t = agent.Post (IRC t)
let fromConsole t = agent.Post (Console t)