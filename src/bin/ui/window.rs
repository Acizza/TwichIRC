use ui::ncurses::*;
use ui::{Size, Position};

// Wrapping the raw ncurses window pointer in a struct will allow us to send it across threads.
pub struct Window {
    pub id: WINDOW
}

// These trait implementations must be marked as unsafe because we're dealing with a raw pointer.
// While ncurses is NOT thread safe, there (hopefully) shouldn't be any issues in this program's use case.
unsafe impl Send for Window {}
unsafe impl Sync for Window {}

impl Window {
    pub fn new(id: WINDOW) -> Window {
        Window { id: id }
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        delwin(self.id);
    }
}

pub struct BorderWindow {
    pub border: Window,
    pub inner:  Window,
}

impl BorderWindow {
    pub fn new(pos: Position, size: Size, inner_offset: Size) -> BorderWindow {
        let border = newwin(size.height, size.width, pos.y, pos.x);
        box_(border, 0, 0);
        wrefresh(border);

        let inner = derwin(border,
                           size.height - 2,
                           size.width - 2,
                           inner_offset.width,
                           inner_offset.height);

        BorderWindow {
            border: Window::new(border),
            inner:  Window::new(inner),
        }
    }
}