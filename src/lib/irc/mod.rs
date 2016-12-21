pub mod message;

use std::net::TcpStream;
use std::io;
use std::io::{BufReader, Write, BufRead, Error};
use irc::message::{Message, MessageError};

/// Represents an error caused by reading a [`Message`] from the server.
/// [`Message`]: message/enum.Message.html
#[derive(Debug)]
pub enum ReadLineError {
    /// The error is related to the connection.
    Connection(Error),
    /// The error is related to the attempt to parse the message received from the server.
    Message(MessageError),
}

/// Represents a TCP connection to an IRC server.
pub struct Connection {
    /// Utility field to allow easy reading from the server.
    reader: BufReader<TcpStream>,
}

impl Connection {
    /// Connects to the specified IRC server.
    pub fn new(host: &str) -> Result<Connection, io::Error> {
        let socket = TcpStream::connect(host)?;
        Ok(Connection {
            reader: BufReader::new(socket)
        })
    }

    /// Sends login credentials to the IRC server.
    pub fn login(&mut self, username: &str, oauth: &str) -> Result<(), io::Error> {
        let mut socket = self.reader.get_mut();

        writeln!(socket, "USER {} 0 * :{}", username, username)?;
        writeln!(socket, "PASS {}", oauth)?;
        writeln!(socket, "NICK {}", username)?;
        writeln!(socket, "CAP REQ :twitch.tv/membership")?;

        Ok(())
    }

    /// Sends a `&str` to the IRC server immediately.
    pub fn write_line(&mut self, string: &str) -> Result<(), io::Error> {
        let socket = self.reader.get_mut();
        writeln!(socket, "{}", string)?;
        socket.flush()
    }

    /// Reads a line directly from the connected server.
    pub fn read_line_raw(&mut self) -> Result<String, Error> {
        let mut line = String::new();
        self.reader.read_line(&mut line)?;

        Ok(line)
    }

    /// Reads a line from the server and parses a [`Message`] from it.
    /// [`Message`]: message/enum.Message.html
    pub fn read_line(&mut self) -> Result<Message, ReadLineError> {
        match self.read_line_raw() {
            Ok(line) => Message::parse(&line).map_err(ReadLineError::Message),
            Err(err) => Err(ReadLineError::Connection(err)),
        }
    }
}