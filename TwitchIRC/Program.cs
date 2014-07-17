using System;
using System.IO;

namespace TwitchIRC
{
	public class Program
	{
		static void Main(string[] args)
		{
			string username = null;
			string oauth    = null;

			var client = new IrcClient(new TwitchHandler());
			var parser = new ArgParser();

			parser.Add("file",     v => parser.Parse(File.ReadAllLines(v)));
			parser.Add("username", v => username = v);
			parser.Add("oauth",    v => oauth = v);

			parser.Parse(args);

			if(username != null && oauth != null) // Temporary
				client.Connect("irc.twitch.tv", 6667, username, oauth);
		}
	}
}