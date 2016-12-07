use ::util::string::StringUtil;

fn read_message_code(string: &str) -> Option<String> {
    // TODO: Replace with slice pattern syntax when it becomes stable
    let mut words = string.splitn(3, ' ');

    if let Some("PING") = words.next() {
        Some("PING".to_string())
    } else if let Some(code) = words.next() {
        Some(code.to_string())
    } else {
        None
    }
}

pub type Code   = String;
pub type Reason = String;

/// Represents a message parse failure.
#[derive(Debug)]
pub enum MessageError {
    /// No message code found.
    NoCode,
    /// Unknown message code found.
    UnknownCode(Code),
    /// Failed to convert the given input into a [`Message`].
    /// [`Message`]: enum.Message.html
    Parse(Reason),
}

impl From<String> for MessageError {
    fn from(err: String) -> MessageError {
        MessageError::Parse(err)
    }
}

pub type Reply    = String;
pub type Contents = String;
pub type Nick     = String;

/// Represents a message from the IRC server.
#[derive(Debug, Clone)]
pub enum Message {
    /// A message sent by a channel viewer.
    Message { nick: String, channel: String, msg: String},
    /// A viewer who joined a channel.
    Joined { nick: String, channel: String },
    /// A viewer who left a channel.
    Left { nick: String, channel: String },
    /// The list of viewers already in a channel.
    Viewers { channel: String, viewers: Vec<Nick> },
    /// A keepalive check sent by the IRC server.
    /// If "PONG `Reply`" is not sent back within 30 seconds, the connection will be terminated by the server.
    Ping(Reply),
    /// A special message from the IRC server.
    /// Failed login attempts will be responded to with this message type.
    Notice(Contents),
    /// Indicates that the login attempt to the IRC server was successful.
    LoggedIn,
}

impl Message {
    /// Attempts to convert a string to a `Message`.
    ///
    /// # Example
    ///
    /// ```
    /// extern crate twirc;
    /// use twirc::irc::message::{Message, MessageError};
    ///
    /// match Message::parse(":tmi.twitch.tv 004 test_name") {
    ///    Ok(Message::LoggedIn)             => println!("Logged in!"),
    ///    Ok(Message::Notice(content))      => println!("Failed to log in: {}", content),
    ///    Err(MessageError::Parse(reason))  => println!("Error parsing message: {}", reason),
    ///    other => println!("Unhandled type: {:?}", other),
    /// }
    /// ```
    pub fn parse(string: &str) -> Result<Message, MessageError> {
        let get_nick = || string.between(":", "!");
        let code = read_message_code(string).ok_or(MessageError::NoCode)?;

        use self::Message::*;
        
        match code.as_str() {
            "PRIVMSG" => Ok(
                Message {
                    nick:    get_nick()?,
                    channel: string.between("#", " :")?,
                    msg:     string.after(" :")?,
                }
            ),
            "JOIN" => Ok(
                Joined {
                    nick:    get_nick()?,
                    channel: string.after(" #")?,
                }
            ),
            "PART" => Ok(
                Left {
                    nick:    get_nick()?,
                    channel: string.after(" #")?,
                }
            ),
            // List of initial channel viewers
            "353" => Ok(
                Viewers {
                    channel: string.between("#", " :")?,
                    viewers:
                        string
                        .after(" :")?
                        .split(' ')
                        .map(String::from)
                        .collect(),
                }
            ),
            "PING"   => Ok(Ping(string.after("PING ")?)),
            "NOTICE" => Ok(Notice(string.after(" :")?)),
            "004"    => Ok(LoggedIn),
            _        => Err(MessageError::UnknownCode(code)),
        }
    }
}