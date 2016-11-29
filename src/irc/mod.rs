pub mod message;

use std::net::TcpStream;
use std::io;
use std::io::{BufReader, Write};

pub use irc::message::Message;

pub struct Connection {
    pub reader: BufReader<TcpStream>,
}

impl Connection {
    pub fn new(host: &str) -> Result<Connection, io::Error> {
        let socket = TcpStream::connect(host)?;

        Ok(Connection {
            reader: BufReader::new(socket)
        })
    }

    pub fn login(&mut self, username: &str, oauth: &str) -> Result<(), io::Error> {
        let mut socket = self.reader.get_mut();

        writeln!(socket, "USER {} 0 * :{}", username, username)?;
        writeln!(socket, "PASS {}", oauth)?;
        writeln!(socket, "NICK {}", username)?;
        writeln!(socket, "CAP REQ :twitch.tv/membership")?;

        Ok(())
    }

    pub fn write_line(&mut self, string: &str) -> Result<(), io::Error> {
        let socket = self.reader.get_mut();
        writeln!(socket, "{}", string)?;
        socket.flush()
    }
}