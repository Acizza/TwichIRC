use ::util::string::{StrUtil, EitherResult};

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

/// Indicates whether someone has received or lost moderator status to a channel.
#[derive(Debug, Clone)]
pub enum ModStatus {
    Received,
    Lost,
}

/// Represents a message from the IRC server.
#[derive(Debug, Clone)]
pub enum Message {
    /// A chat message has been sent.
    Message { nick: String, channel: String, msg: String},
    /// Someone has joined a channel.
    Joined { nick: String, channel: String },
    /// Someone has left a channel.
    Left { nick: String, channel: String },
    /// The list of people already in a channel.
    Viewers { channel: String, viewers: Vec<Nick> },
    /// A keepalive check sent by the IRC server.
    /// If "PONG `Reply`" is not sent back within 30 seconds, the connection will be terminated by the server.
    Ping(Reply),
    /// A special message from the IRC server (usually indicates a failed login attempt.)
    Notice(Contents),
    /// A login attempt was successful.
    LoggedIn,
    /// Someone has either received or lost moderator status to a channel.
    Moderator { nick: String, channel: String, status: ModStatus },
}

impl Message {
    /// Attempts to convert a string to a `Message`.
    ///
    /// # Example
    ///
    /// ```
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
            // Someone received / lost moderator status
            "MODE"   => Ok(
                Moderator {
                    nick:    string.after("o ")?,
                    channel: string.between("#", " ")?,
                    status:
                        match string.either("+o", "-o")? {
                            EitherResult::First  => ModStatus::Received,
                            EitherResult::Second => ModStatus::Lost,
                        }
                }
            ),
            _        => Err(MessageError::UnknownCode(code)),
        }
    }
}