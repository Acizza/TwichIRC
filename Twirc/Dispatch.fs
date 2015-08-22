module Dispatch

type Type =
    | IRC of Message.MessageType
    | Command of string
    | NewState of Client.State

let private agent = MailboxProcessor.Start (fun inbox ->
    let rec loop state = async {
        let! msg = inbox.Receive()

        let nextState =
            match msg with
            | IRC msg -> Client.processMessage msg state
            | Command str -> Command.execute str state
            | NewState s -> s

        return! loop nextState
    }
    
    loop Client.State.Zero
)

let fromType t = agent.Post t
let fromIRC t = agent.Post (IRC t)
let fromCommand t = agent.Post (Command t)
let fromState t = agent.Post (NewState t)