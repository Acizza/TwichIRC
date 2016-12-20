use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, STATS_PANEL_HEIGHT, Window, Size};

pub struct ChannelList {
    parent: Window,
    child:  Window,
}

impl ChannelList {
    pub fn new(size: Size) -> ChannelList {
        let parent = newwin(
                size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT,
                RIGHT_PANEL_WIDTH,
                0,
                size.width - RIGHT_PANEL_WIDTH);
        box_(parent, 0, 0);
        wrefresh(parent);

        let child = derwin(parent,
                           size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT,
                           RIGHT_PANEL_WIDTH - 2,
                           1,
                           1);
        
        ChannelList {
            parent: Window::new(parent),
            child:  Window::new(child),
        }
    }
}