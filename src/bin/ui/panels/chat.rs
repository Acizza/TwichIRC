use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, Position, Size};
use ui::window::BorderWindow;

pub struct Chat {
    window: BorderWindow,
}

// glibc will panic if a message containing a C-style format string is entered
fn sanitize(string: &str) -> String {
    string.replace('%', "%%")
}

impl Chat {
    pub fn new(size: Size) -> Chat {
        let window = BorderWindow::new(
                        Position::new(0, 0),
                        Size::new(size.width - RIGHT_PANEL_WIDTH, size.height - CMD_ENTRY_HEIGHT),
                        Size::new(1, 1));

        scrollok(window.inner.id, true);

        Chat { window: window }
    }

    pub fn add_message(&self, msg: &str) {
        wprintw(self.window.inner.id, &format!("{}\n", sanitize(msg)));
        wrefresh(self.window.inner.id);
    }
}