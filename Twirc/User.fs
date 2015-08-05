module User

open Result

type User = {
    name: string;
    dataLink: DataLink.Link;
}

let createAndLogin nick oauth uplink =
    let send = DataLink.queueLine uplink
    send ""
    send (sprintf "USER %s 0 * :%s" nick nick)
    send (sprintf "PASS %s" oauth)
    send (sprintf "NICK %s" nick)
    DataLink.flush uplink

    {
        name = nick;
        dataLink = uplink;
    }

let joinChannel user channel =
    DataLink.sendLine user.dataLink (sprintf "JOIN #%s" channel)