pub mod message;

use std::net::TcpStream;
use std::io;
use std::io::{BufReader, Write, BufRead, Error};
use irc::message::{Message, MessageError};

/// Contains methods that handle common IRC functionality.
pub trait Irc {
    /// Sends login credentials to the server.
    fn login(&mut self, username: &str, oauth: &str) -> Result<(), io::Error>;
}

impl Irc for TcpStream {
    fn login(&mut self, username: &str, oauth: &str) -> Result<(), io::Error> {
        writeln!(self, "USER {} 0 * :{}", username, username)?;
        writeln!(self, "PASS {}", oauth)?;
        writeln!(self, "NICK {}", username)?;
        writeln!(self, "CAP REQ :twitch.tv/membership")?;

        Ok(())
    }
}

pub trait StreamUtil {
    /// Sends a `&str` to the server immediately.
    fn send_line(&mut self, string: &str) -> Result<(), io::Error>;
}

impl StreamUtil for TcpStream {
   fn send_line(&mut self, string: &str) -> Result<(), io::Error> {
        writeln!(self, "{}", string)?;
        self.flush()
    }
}

/// Represents an error caused by reading a [`Message`] from the server.
/// [`Message`]: message/enum.Message.html
#[derive(Debug)]
pub enum ReadLineError {
    /// The error is related to the connection.
    Connection(Error),
    /// The error is related to the attempt to parse the message received from the server.
    Message(MessageError),
}

pub trait ReaderUtil {    
    /// Reads a line directly from the connected server.
    fn read_line_raw(&mut self) -> Result<String, Error>;

    /// Reads a line from the server and parses a [`Message`] from it.
    /// [`Message`]: message/enum.Message.html
    fn read_message(&mut self) -> Result<Message, ReadLineError>;
}

impl ReaderUtil for BufReader<TcpStream> {
    fn read_line_raw(&mut self) -> Result<String, Error> {
        let mut line = String::new();
        self.read_line(&mut line)?;

        Ok(line)
    }

    fn read_message(&mut self) -> Result<Message, ReadLineError> {
        self.read_line_raw()
            .map_err(ReadLineError::Connection)
            .and_then(|line| Message::parse(&line).map_err(ReadLineError::Message))
    }
}