extern crate twirc;

mod ui;

use std::env;
use std::io::BufRead;
use std::thread;
use twirc::irc::message::Message;
use twirc::irc::Connection;
use ui::UI;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::sync::Arc;

// Please note that all of this is temporary.

enum MsgSource {
    IRC(String),
    Terminal(char),
}

fn handle_irc(tx: &Sender<MsgSource>, conn: Connection) {
    let tx = tx.clone();

    thread::spawn(move || {
        for line in conn.reader.lines() {
            let line = line.expect("failed to read line");
            tx.send(MsgSource::IRC(line)).unwrap();
        }
    });
}

fn handle_terminal(tx: &Sender<MsgSource>, ui: &Arc<UI>) {
    let tx = tx.clone();
    let ui = ui.clone();

    thread::spawn(move || {
        loop {
            match ui.command_entry.read_input_char() {
                Some(ch) => tx.send(MsgSource::Terminal(ch)).unwrap(),
                None     => (),
            }
        }
    });
}

fn handle_events(rx: Receiver<MsgSource>, ui: &Arc<UI>) {
    use MsgSource::*;

    loop {
        match rx.recv().unwrap() {
            IRC(line)    => ui.chat.add_message(&format!("{:?}", Message::parse(&line))),
            Terminal(ch) => ui.command_entry.process_char(ch),
        }
    }
}

fn main() {
    let mut args = env::args().skip(1);

    let nick  = args.next().unwrap();
    let oauth = args.next().unwrap();
    let channels = args.collect::<Vec<_>>();

    let mut conn = Connection::new("irc.chat.twitch.tv:6667").unwrap();
    conn.login(&nick, &oauth).unwrap();

    for channel in channels {
        conn.write_line(&format!("JOIN #{}", channel)).unwrap();
    }

    let ui = Arc::new(UI::create());
    let (tx, rx) = channel();

    handle_irc(&tx, conn);
    handle_terminal(&tx, &ui);

    handle_events(rx, &ui);
}