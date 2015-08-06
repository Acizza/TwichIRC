module IRC

let sendLogin nick oauth uplink =
    let send = DataLink.queueLine uplink
    send ""
    send (sprintf "USER %s 0 * :%s" nick nick)
    send (sprintf "PASS %s" oauth)
    send (sprintf "NICK %s" nick)
    DataLink.flush uplink

let inline joinChannel uplink channel     = DataLink.sendLine uplink (sprintf "JOIN #%s" channel)
let inline leaveChannel uplink channel    = DataLink.sendLine uplink (sprintf "PART #%s" channel)
let inline sendMessage uplink channel msg = DataLink.sendLine uplink (sprintf "PRIVMSG #%s :%s" channel msg)