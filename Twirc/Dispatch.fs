module Dispatch

type Type =
    | IRC of Message.MessageType
    | Console of string
    | NewState of Client.State

let private agent = MailboxProcessor.Start (fun inbox ->
    let rec loop state = async {
        let! msg = inbox.Receive()

        let nextState =
            match msg with
            | IRC msg -> Client.processMessage msg state
            | Console str -> Console.processMessage str state
            | NewState s -> s

        return! loop nextState
    }
    
    loop Client.State.Zero
)

let fromType t = agent.Post t
let fromIRC t = agent.Post (IRC t)
let fromConsole t = agent.Post (Console t)
let fromState t = agent.Post (NewState t)