using System;

namespace TwitchIRC
{
	public abstract class AClientHandler
	{
		public IrcClient Client { get; set; }

		public abstract void ProcessLine(string line);
	}
}