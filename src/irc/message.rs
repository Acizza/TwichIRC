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

#[derive(Debug)]
pub enum MessageError {
    NoCode,
    UnknownCode(Code),
    Parse(Reason),
}

impl From<String> for MessageError {
    fn from(err: String) -> MessageError {
        MessageError::Parse(err)
    }
}

pub type Reply    = String;
pub type Contents = String;

#[derive(Debug)]
pub enum Message {
    Message { nick: String, channel: String, msg: String},
    Join    { nick: String, channel: String },
    Leave   { nick: String, channel: String },
    Ping(Reply),
    Notice(Contents),
}

impl Message {
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
                Join {
                    nick:    get_nick()?,
                    channel: string.after(" #")?,
                }
            ),
            "PART" => Ok(
                Leave {
                    nick:    get_nick()?,
                    channel: string.after(" #")?,
                }
            ),
            "PING"   => Ok(Ping(string.after("PING ")?)),
            "NOTICE" => Ok(Notice(string.after(" :")?)),
            _        => Err(MessageError::UnknownCode(code)),
        }
    }
}