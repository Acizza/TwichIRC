extern crate twirc;

mod ui;
mod client;

use std::env;
use client::Client;

fn main() {
    let mut args = env::args().skip(1);

    let nick  = args.next().unwrap();
    let oauth = args.next().unwrap();
    let channels = args.collect::<Vec<_>>();

    let mut client = Client::new(&nick, &oauth).unwrap();

    for channel in channels {
        client.join_channel(&channel).unwrap();
    }

    client.begin_processing().unwrap();
}