use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, STATS_PANEL_HEIGHT, Position, Size};
use ui::window::BorderWindow;

pub struct ChannelList {
    window: BorderWindow,
}

impl ChannelList {
    pub fn new(size: Size) -> ChannelList {
        let window = BorderWindow::new(
                        Position::new(size.width - RIGHT_PANEL_WIDTH, 0),
                        Size::new(RIGHT_PANEL_WIDTH, size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT),
                        Size::new(1, 1));
        
        ChannelList { window: window }
    }
}