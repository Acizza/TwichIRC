use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, Position, Size};
use ui::window::BorderWindow;

pub struct Chat {
    window: BorderWindow,
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
        waddstr(self.window.inner.id, &format!("{}\n", msg));
        wrefresh(self.window.inner.id);
    }
}