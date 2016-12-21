extern crate twirc;

mod ui;

use std::env;
use std::thread;
use twirc::irc::message::Message;
use twirc::irc::Connection;
use ui::UI;
use std::sync::mpsc::{channel, Sender, Receiver};

// Please note that all of this is temporary.

enum MsgSource {
    IRC(Message),
    Terminal(char),
    Error(String),
}

fn handle_irc(tx: &Sender<MsgSource>, conn: Connection) {
    let tx = tx.clone();
    let mut conn = conn;

    thread::spawn(move || {
        loop {
            match conn.read_line() {
                Ok(Message::Ping(reply)) => conn.write_line(&format!("PONG {}", reply)).unwrap(),
                Ok(msg)  => tx.send(MsgSource::IRC(msg)).unwrap(),
                Err(err) => tx.send(MsgSource::Error(format!("{:?}", err))).unwrap(),
            }
        }
    });
}

fn handle_terminal(tx: &Sender<MsgSource>) {
    let tx = tx.clone();

    thread::spawn(move || {
        loop {
            match UI::read_char() {
                Some(ch) => tx.send(MsgSource::Terminal(ch)).unwrap(),
                None     => (),
            }
        }
    });
}

fn handle_events(rx: Receiver<MsgSource>, ui: &UI) {
    use MsgSource::*;

    loop {
        match rx.recv().unwrap() {
            IRC(msg)     => ui.chat.add_message(&format!("{:?}", msg)),
            Terminal(ch) => ui.process_char(ch),
            Error(msg)   => ui.chat.add_message(&format!("ERROR: {}", msg)),
        }
    }
}

fn main() {
    let mut args = env::args().skip(1);

    let nick  = args.next().unwrap();
    let oauth = args.next().unwrap();
    let channels = args.collect::<Vec<_>>();

    let mut ui = UI::create();

    let mut conn = Connection::new("irc.chat.twitch.tv:6667").unwrap();
    conn.login(&nick, &oauth).unwrap();

    for channel in channels {
        conn.write_line(&format!("JOIN #{}", channel)).unwrap();
        ui.channel_list.add_channel(&channel);
    }

    let (tx, rx) = channel();

    handle_irc(&tx, conn);
    handle_terminal(&tx);

    handle_events(rx, &ui);
}