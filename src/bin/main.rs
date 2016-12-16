extern crate twirc;

mod ui;

use std::env;
use std::io::BufRead;
use twirc::irc::message::Message;
use twirc::irc::Connection;
use ui::UI;

fn read_incoming(conn: Connection, ui: &UI) {
    for line in conn.reader.lines() {
        let line = line.expect("failed to read line");
        ui.chat.add_message(&format!("{:?}", Message::parse(&line)));
    }
}

fn main() {
    // Temporary
    let mut args = env::args().skip(1);

    let nick  = args.next().unwrap();
    let oauth = args.next().unwrap();
    let channels = args.collect::<Vec<_>>();

    let mut conn = Connection::new("irc.chat.twitch.tv:6667").unwrap();
    conn.login(&nick, &oauth).unwrap();

    for channel in channels {
        conn.write_line(&format!("JOIN #{}", channel)).unwrap();
    }

    let ui = UI::create();
    read_incoming(conn, &ui);
}