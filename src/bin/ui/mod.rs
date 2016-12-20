extern crate ncurses;

mod panels;
mod window;

use self::ncurses::*;
use self::panels::chat::Chat;
use self::panels::command_entry::CommandEntry;
use self::panels::channel_list::ChannelList;

use self::window::Window;

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
        keypad(stdscr(), true);

        // Manually refresh stdscr now to prevent it from overwriting other windows later on
        refresh();

        let term_size = get_window_size(stdscr());

        UI {
            chat:          Chat::new(term_size),
            command_entry: CommandEntry::new(term_size),
            channel_list:  ChannelList::new(term_size),
            channel_stats: UI::create_channel_stats(term_size),
        }
    }

    pub fn read_char() -> Option<char> {
        match getch() {
            -1 => None,
            i  => ::std::char::from_u32(i as u32),
        }
    }

    pub fn process_char(&self, ch: char) {
        self.command_entry.process_char(ch);
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