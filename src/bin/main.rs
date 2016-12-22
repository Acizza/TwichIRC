extern crate twirc;

mod ui;

use std::env;
use std::thread;
use std::net::TcpStream;
use std::io::{BufReader, Write};
use std::sync::mpsc::{channel, Sender, Receiver};
use twirc::irc::message::Message;
use twirc::irc::{Irc, StreamUtil, ReaderUtil};
use ui::UI;

// Please note that all of this is temporary.

enum MsgSource {
    IRC(Message),
    Terminal(char),
    Error(String),
}

fn handle_irc(tx: &Sender<MsgSource>, mut reader: BufReader<TcpStream>) {
    let tx = tx.clone();

    thread::spawn(move || {
        loop {
            match reader.read_message() {
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

fn handle_events(rx: Receiver<MsgSource>, ui: &UI, mut writer: TcpStream) {
    use MsgSource::*;

    loop {
        match rx.recv().unwrap() {
            IRC(msg) => {
                match msg {
                    Message::Ping(reply) => writeln!(writer, "PONG {}", reply).unwrap(),
                    msg => ui.chat.add_message(&format!("{:?}", msg)),
                }
            },
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

    let mut conn = TcpStream::connect("irc.chat.twitch.tv:6667").unwrap();
    conn.login(&nick, &oauth).unwrap();

    for channel in channels {
        conn.send_line(&format!("JOIN #{}", channel)).unwrap();
        ui.channel_list.add_channel(&channel);
    }

    let (tx, rx) = channel();

    let conn_clone = conn.try_clone().unwrap();
    handle_irc(&tx, BufReader::new(conn));
    handle_terminal(&tx);

    handle_events(rx, &ui, conn_clone);
}