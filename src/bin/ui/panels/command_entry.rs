use ui;
use ui::ncurses::*;
use ui::{CMD_ENTRY_HEIGHT, Window, Size, Position};
use std::cmp;

pub struct CommandEntry {
    parent: Window,
    child:  Window,
}

// char::is_control() checks for escape sequences,
// so we need to use our own version that works with ncurse's keypad() function
fn is_control_char(ch: char) -> bool {
    let ch = ch as i32;
    ch >= KEY_MIN && ch <= KEY_MAX
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

    fn move_cursor(&self, x_pos: i32) {
        wmove(self.child.id, 0, cmp::max(x_pos, 0));
        wrefresh(self.child.id);
    }

    pub fn process_char(&self, ch: char) {
        if is_control_char(ch) {
            match ch as i32 {
                KEY_BACKSPACE => self.backspace_key(),
                KEY_HOME      => self.move_cursor(0),
                KEY_DC        => self.delete_key(),
                _             => (),
            }
        } else {
            wechochar(self.child.id, ch as u32);
        }
    }

    fn delete_char_at(&self, index: i32) {
        self.move_cursor(index);
        
        wdelch(self.child.id);
        wrefresh(self.child.id);
    }

    fn backspace_key(&self) {
        let Position { x, .. } = ui::get_cursor_pos(self.child.id);

        if x > 0 {
            self.delete_char_at(x - 1);
        }
    }

    fn delete_key(&self) {
        let Position { x, .. } = ui::get_cursor_pos(self.child.id);
        self.delete_char_at(x);
    }
}