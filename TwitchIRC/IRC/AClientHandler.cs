using System;
using System.Collections.Generic;

namespace TwitchIRC
{
	public abstract class AClientHandler
	{
		public IrcClient Client { get; set; }
		public ACommandHandler CommandHandler { get; protected set; }

		public abstract void ProcessLine(string line);
	}
}