use ui::ncurses::*;
use ui::{RIGHT_PANEL_WIDTH, CMD_ENTRY_HEIGHT, STATS_PANEL_HEIGHT, Position, Size};
use ui::window::BorderWindow;

pub struct ChannelList {
    window:   BorderWindow,
    channels: Vec<String>,
}

impl ChannelList {
    pub fn new(size: Size) -> ChannelList {
        let window = BorderWindow::new(
                        Position::new(size.width - RIGHT_PANEL_WIDTH, 0),
                        Size::new(RIGHT_PANEL_WIDTH, size.height - STATS_PANEL_HEIGHT - CMD_ENTRY_HEIGHT),
                        Size::new(1, 1));
        
        ChannelList {
            window: window,
            channels: Vec::new(),
        }
    }

    fn display_channel(&self, index: i32, name: &str) {
        mvwaddstr(self.window.inner.id,
                  index,
                  1,
                  &format!("{} - {}", index + 1, name));
        wrefresh(self.window.inner.id);
    }

    pub fn add_channel(&mut self, name: &str) {
        self.channels.push(name.to_string());
        self.display_channel(self.channels.len() as i32 - 1, name);
    }

    pub fn remove_channel(&mut self, name: &str) -> bool {
        let index = self.channels.iter().position(|ref s| s.as_str() == name);

        match index {
            Some(index) => {
                self.channels.remove(index);

                // TODO: Separate this redraw code into its own function once this method is actually used
                wclear(self.window.inner.id);

                for (index, channel) in self.channels.iter().by_ref().enumerate() {
                    self.display_channel(index as i32, &channel);
                }

                true
            },
            None => false,
        }
    }
}