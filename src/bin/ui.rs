extern crate ncurses;

use self::ncurses::*;

pub const RIGHT_PANEL_WIDTH: i32  = 30;
pub const CMD_ENTRY_HEIGHT: i32   = 3;
pub const STATS_PANEL_HEIGHT: i32 = 20;

#[derive(Debug, Copy, Clone)]
pub struct Size {
    width: i32,
    height: i32,
}

impl Size {
    pub fn new(width: i32, height: i32) -> Size {
        Size {
            width: width,
            height: height,
        }
    }
}

// Wrapping the raw ncurses window pointer in a struct will allow us to send it across threads.
pub struct Window {
    id: WINDOW
}

// This must be marked as unsafe because we're dealing with a raw pointer.
// While ncurses is NOT thread safe, there (hopefully) shouldn't be any issues in this program's use case.
unsafe impl Send for Window {}

impl Window {
    fn new(id: WINDOW) -> Window {
        Window { id: id }
    }

    fn get_size_raw(window: WINDOW) -> Size {
        let mut x = 0;
        let mut y = 0;
        getmaxyx(window, &mut y, &mut x);
        Size::new(x, y)
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        delwin(self.id);
    }
}

pub struct Chat {
    parent: Window,
    child:  Window,
}

impl Chat {
    fn new(size: Size) -> Chat {
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
        wprintw(self.child.id, &format!("{}\n", msg));
        wrefresh(self.child.id);
    }
}

pub struct UI {
    pub chat:          Chat,
    pub command_entry: Window,
    pub channel_list:  Window,
    pub channel_stats: Window,
}

impl UI {
    fn create_command_entry(size: Size) -> Window {
        let w = newwin(CMD_ENTRY_HEIGHT, size.width, size.height - CMD_ENTRY_HEIGHT, 0);
        box_(w, 0, 0);
        wrefresh(w);
        Window::new(w)
    }

    fn create_channel_list(size: Size) -> Window {
        let w = newwin(size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT,
                       RIGHT_PANEL_WIDTH,
                       0,
                       size.width - RIGHT_PANEL_WIDTH);
        box_(w, 0, 0);
        wrefresh(w);
        Window::new(w)
    }

    fn create_channel_stats(size: Size) -> Window {
        let w = newwin(STATS_PANEL_HEIGHT,
                       RIGHT_PANEL_WIDTH,
                       size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT,
                       size.width - RIGHT_PANEL_WIDTH);
        box_(w, 0, 0);
        wrefresh(w);
        Window::new(w)
    }

    pub fn create() -> UI {
        initscr();
        start_color();
        curs_set(CURSOR_VISIBILITY::CURSOR_INVISIBLE);

        let term_size = Window::get_size_raw(stdscr());

        UI {
            chat:          Chat::new(term_size),
            command_entry: UI::create_command_entry(term_size),
            channel_list:  UI::create_channel_list(term_size),
            channel_stats: UI::create_channel_stats(term_size),
        }
    }

    pub fn read_input() -> Option<char> {
        match getch() {
            -1 => None,
            i  => ::std::char::from_u32(i as u32),
        }
    }
}

impl Drop for UI {
    fn drop(&mut self) {
        endwin();
    }
}