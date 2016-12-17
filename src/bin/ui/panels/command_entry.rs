use ui::ncurses::*;
use ui::{CMD_ENTRY_HEIGHT, Window, Size};

pub struct CommandEntry {
    parent: Window,
    child:  Window,
}

impl CommandEntry {
    pub fn new(size: Size) -> CommandEntry {
        let parent = newwin(CMD_ENTRY_HEIGHT,
                            size.width,
                            size.height - CMD_ENTRY_HEIGHT,
                            0);
        box_(parent, 0, 0);
        wrefresh(parent);

        let child = derwin(parent,
                           CMD_ENTRY_HEIGHT - 2,
                           size.width - 2,
                           1,
                           1);
        scrollok(child, true);

        CommandEntry {
            parent: Window::new(parent),
            child:  Window::new(child),
        }
    }

    pub fn read_input_char(&self) -> Option<char> {
        match wgetch(self.child.id) {
            -1 => None,
            i  => ::std::char::from_u32(i as u32),
        }
    }

    pub fn add_char(&self, ch: char) {
        wprintw(self.child.id, &ch.to_string());
        wrefresh(self.child.id);
    }
}