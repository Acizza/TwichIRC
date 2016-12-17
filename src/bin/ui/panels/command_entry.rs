use ui;
use ui::ncurses::*;
use ui::{CMD_ENTRY_HEIGHT, Window, Size};
use std::cmp;

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
        keypad(child, true);

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

    pub fn process_char(&self, ch: char) {
        match ch as i32 {
            KEY_BACKSPACE => self.backspace_key(),
            KEY_HOME      => { wmove(self.child.id, 0, 0); },
            KEY_DC        => self.delete_key(),
            KEY_UP | KEY_DOWN => (),
            _ => { wechochar(self.child.id, ch as u32); },
        }
    }

    fn delete_char_at(&self, index: i32) {
        let old_pos = ui::get_cursor_pos(self.child.id);

        wmove(self.child.id, 0, cmp::max(index, 0));
        wdelch(self.child.id);
        wmove(self.child.id, 0, old_pos.x - 1);
        wrefresh(self.child.id);
    }

    fn backspace_key(&self) {
        let pos = ui::get_cursor_pos(self.child.id);

        if pos.x > 0 {
            self.delete_char_at(pos.x - 1);
        }
    }

    fn delete_key(&self) {
        let pos = ui::get_cursor_pos(self.child.id);
        self.delete_char_at(pos.x);
    }
}