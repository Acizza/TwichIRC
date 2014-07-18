using System;
using System.Collections.Generic;

namespace TwitchIRC
{
	public abstract class AClientHandler
	{
		public IrcClient Client { get; set; }
		public ACommandHandler CommandHandler { get; protected set; }

		/// <summary>
		/// Called after Client has been initialized.
		/// </summary>
		public abstract void PostInit();

		public abstract void OnConnect(string user, string host, int port);
		public abstract void OnJoin(string user, string channel);
		public abstract void OnMessage(string user, string channel, string msg);
		public abstract void OnLeave(string user, string channel);
		public abstract void OnDisconnect(string user);
		public abstract void OnUnknown(string line);
	}
}