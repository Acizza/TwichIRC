use std::error::Error;
use std::io;
use std::io::BufReader;
use std::thread;
use std::net::TcpStream;
use std::sync::mpsc::{channel, Sender, Receiver, RecvError};
use twirc::irc::{Irc, StreamUtil, ReaderUtil};
use twirc::irc::message::Message;
use ui::UI;

pub enum EventType {
    IRC(Message),
    Input(char),
    Error(String),
}

pub struct Client {
    conn:    TcpStream,
    ui:      UI,
    event:   Sender<EventType>,
    ev_recv: Receiver<EventType>,
}

impl Client {
    pub fn new(username: &str, oauth: &str) -> Result<Client, io::Error> {
        let mut conn = TcpStream::connect("irc.chat.twitch.tv:6667")?;
        conn.login(username, oauth)?;

        let (sender, receiver) = channel();

        Ok(Client {
            conn:    conn,
            ui:      UI::create(),
            event:   sender,
            ev_recv: receiver,
        })
    }

    pub fn join_channel(&mut self, channel: &str) -> Result<(), io::Error> {
        self.conn.send_line(&format!("JOIN #{}", channel))?;
        self.ui.channel_list.add_channel(&channel);

        Ok(())
    }

    // TODO: Separate (?)
    fn process_incoming_irc(&mut self) -> Result<(), io::Error> {
        let tx = self.event.clone();
        let mut reader = BufReader::new(self.conn.try_clone()?);

        thread::spawn(move || {
            loop {
                match reader.read_message() {
                    Ok(msg)  => tx.send(EventType::IRC(msg)).unwrap(),
                    Err(err) => tx.send(EventType::Error(format!("{:?}", err))).unwrap(),
                }
            }
        });

        Ok(())
    }

    // TODO: Separate (?)
    fn process_incoming_input(&self) {
        let tx = self.event.clone();

        thread::spawn(move || {
            loop {
                match UI::read_char() {
                    Some(ch) => tx.send(EventType::Input(ch)).unwrap(),
                    None     => (),
                }
            }
        });
    }

    fn handle_irc_msg(&mut self, msg: Message) {
        use twirc::irc::message::Message::*;

        match msg {
            Ping(reply) => self.conn.send_line(&format!("PONG {}", reply)).unwrap(),
            other       => self.ui.chat.add_message(&format!("{:?}", other)),
        }
    }

    fn process_events(&mut self) -> Result<(), RecvError> {
        use self::EventType::*;

        loop {
            match self.ev_recv.recv()? {
                IRC(msg)   => self.handle_irc_msg(msg),
                Input(ch)  => self.ui.process_char(ch),
                Error(err) => self.ui.chat.add_message(&format!("ERROR: {}", err)),
            }
        }
    }

    pub fn begin_processing(&mut self) -> Result<(), Box<Error>> {
        self.process_incoming_irc()?;
        self.process_incoming_input();

        self.process_events()?;
        Ok(())
    }
}