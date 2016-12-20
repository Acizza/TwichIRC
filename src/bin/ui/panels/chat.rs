use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, Window, Size};

pub struct Chat {
    parent: Window,
    child:  Window,
}

// glibc will panic if a message containing a C-style format string is entered
fn sanitize(string: &str) -> String {
    string.replace('%', "%%")
}

impl Chat {
    pub fn new(size: Size) -> Chat {
        let parent = newwin(size.height - CMD_ENTRY_HEIGHT,
                            size.width - RIGHT_PANEL_WIDTH,
                            0,
                            0);
        box_(parent, 0, 0);
        wrefresh(parent);

        let child = derwin(parent,
                           size.height - CMD_ENTRY_HEIGHT - 2,
                           size.width - RIGHT_PANEL_WIDTH - 2,
                           1,
                           1);
        scrollok(child, true);

        Chat {
            parent: Window::new(parent),
            child:  Window::new(child),
        }
    }

    pub fn add_message(&self, msg: &str) {
        wprintw(self.child.id, &format!("{}\n", sanitize(msg)));
        wrefresh(self.child.id);
    }
}