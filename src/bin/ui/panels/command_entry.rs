use ui;
use ui::ncurses::*;
use ui::{CMD_ENTRY_HEIGHT, Size, Position};
use ui::window::BorderWindow;
use std::cmp;

pub struct CommandEntry {
    window: BorderWindow
}

// char::is_control() checks for escape sequences,
// so we need to use our own version that works with ncurses' keypad() function
fn is_control_char(ch: char) -> bool {
    let ch = ch as i32;
    ch >= KEY_MIN && ch <= KEY_MAX
}

impl CommandEntry {
    pub fn new(size: Size) -> CommandEntry {
        let window = BorderWindow::new(
                        Position::new(0, size.height - CMD_ENTRY_HEIGHT),
                        Size::new(size.width, CMD_ENTRY_HEIGHT),
                        Size::new(1, 1));
        
        scrollok(window.inner.id, true);
        keypad(window.inner.id, true);

        CommandEntry { window: window }
    }

    pub fn read_input_char(&self) -> Option<char> {
        match wgetch(self.window.inner.id) {
            -1 => None,
            i  => ::std::char::from_u32(i as u32),
        }
    }

    fn move_cursor(&self, x_pos: i32) {
        wmove(self.window.inner.id, 0, cmp::max(x_pos, 0));
        wrefresh(self.window.inner.id);
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
            wechochar(self.window.inner.id, ch as u32);
        }
    }

    fn delete_char_at(&self, index: i32) {
        self.move_cursor(index);
        
        wdelch(self.window.inner.id);
        wrefresh(self.window.inner.id);
    }

    fn backspace_key(&self) {
        let Position { x, .. } = ui::get_cursor_pos(self.window.inner.id);

        if x > 0 {
            self.delete_char_at(x - 1);
        }
    }

    fn delete_key(&self) {
        let Position { x, .. } = ui::get_cursor_pos(self.window.inner.id);
        self.delete_char_at(x);
    }
}