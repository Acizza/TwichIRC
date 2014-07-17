using System;

namespace TwitchIRC
{
	public class TwitchHandler : AClientHandler
	{
		public TwitchHandler()
		{
		}

		public override void ProcessLine(string line)
		{
			Console.WriteLine(line);
		}
	}
}