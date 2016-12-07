extern crate twirc;

use std::env;
use std::io::BufRead;
use twirc::irc::message::Message;
use twirc::irc::Connection;

fn read_incoming(conn: Connection) {
    for line in conn.reader.lines() {
        let line = line.expect("failed to read line");
        println!("{}\n^ {:?}", line, Message::parse(&line));
    }

    println!("connection ended");
}

fn main() {
    // Temporary
    let mut args = env::args().skip(1);

    let nick  = args.next().unwrap();
    let oauth = args.next().unwrap();
    let channels = args.collect::<Vec<_>>();

    let mut conn = Connection::new("irc.chat.twitch.tv:6667").unwrap();
    conn.login(&nick, &oauth).unwrap();

    println!("connected & logged in");

    for channel in channels {
        conn.write_line(&format!("JOIN #{}", channel)).unwrap();
    }

    read_incoming(conn);
}