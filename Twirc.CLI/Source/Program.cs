﻿using System;
using Twirc.Lib;

namespace Twirc.CLI
{
	public sealed class Program
	{
		static void Main(string[] args)
		{
			using(var irc = new IRCClient("irc.twitch.tv", 6667))
			{
				irc.OnLogin += (response, username, password) =>
				{
					if(response.Status == LoginStatus.Success)
						Console.WriteLine("Logged in as: " + username);
					else
						Console.WriteLine("Failed to login [{0}]: {1}", response.Code, response.Message);
				};

				irc.OnPing += () =>
				{
					Console.WriteLine("Received PING request");
				};

				irc.OnMessage += (channel, user, message) =>
				{
					Console.WriteLine("<{0}> {1}: {2}", channel, user, message);
				};

				irc.Login("<username>", "<oauth>");
				irc.Join("<channel here>");

				int counter = 0;

				while(counter++ < 100)
					irc.ProcessNextLine();
			}
		}
	}
}