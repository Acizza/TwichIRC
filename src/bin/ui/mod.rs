extern crate ncurses;

mod panels;

use self::ncurses::*;
use self::panels::chat::Chat;
use self::panels::command_entry::CommandEntry;
use self::panels::channel_list::ChannelList;

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

#[derive(Debug, Copy, Clone)]
pub struct Position {
    x: i32,
    y: i32,
}

impl Position {
    pub fn new(x: i32, y: i32) -> Position {
        Position {
            x: x,
            y: y,
        }
    }
}

// Wrapping the raw ncurses window pointer in a struct will allow us to send it across threads.
pub struct Window {
    id: WINDOW
}

// These trait implementations must be marked as unsafe because we're dealing with a raw pointer.
// While ncurses is NOT thread safe, there (hopefully) shouldn't be any issues in this program's use case.
unsafe impl Send for Window {}
unsafe impl Sync for Window {}

impl Window {
    fn new(id: WINDOW) -> Window {
        Window { id: id }
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        delwin(self.id);
    }
}

pub struct UI {
    pub chat:          Chat,
    pub command_entry: CommandEntry,
    pub channel_list:  ChannelList,
    pub channel_stats: Window,
}

impl UI {
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

        noecho();
        curs_set(CURSOR_VISIBILITY::CURSOR_INVISIBLE);

        let term_size = get_window_size(stdscr());

        UI {
            chat:          Chat::new(term_size),
            command_entry: CommandEntry::new(term_size),
            channel_list:  ChannelList::new(term_size),
            channel_stats: UI::create_channel_stats(term_size),
        }
    }
}

impl Drop for UI {
    fn drop(&mut self) {
        endwin();
    }
}

fn get_window_size(window: WINDOW) -> Size {
    let mut x = 0;
    let mut y = 0;
    getmaxyx(window, &mut y, &mut x);
    Size::new(x, y)
}

fn get_cursor_pos(window: WINDOW) -> Position {
    let mut x = 0;
    let mut y = 0;
    getyx(window, &mut y, &mut x);
    Position::new(x, y)
}