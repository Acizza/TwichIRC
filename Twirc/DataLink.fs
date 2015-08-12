module DataLink

// Purpose: Provides an abstracted interface to read/write from/to the server.

open System.IO
open System.Text
open System.Net.Sockets

type Link = {
    client: TcpClient;
    reader: StreamReader;
    writer: StreamWriter;
} with
    static member Zero = {
        client = null;
        reader = null;
        writer = null;
    }

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b

let create ip port =
    try
        let client = new TcpClient(ip, port)
        let reader = new StreamReader(client.GetStream(), Encoding.UTF8)
        let writer = new StreamWriter(client.GetStream(), Encoding.UTF8)

        writer.NewLine <- "\r\n"

        Success {
            client = client;
            reader = reader;
            writer = writer;
        }
    with ex -> Failure ex.Message

let queueLine uplink (line:string) =
    uplink.writer.WriteLine line

let flush uplink =
    uplink.writer.Flush()

let sendLine uplink (line:string) =
    queueLine uplink line
    flush uplink

let readLine uplink =
    uplink.reader.ReadLine()