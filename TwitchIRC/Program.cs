using System;

namespace TwitchIRC
{
	public class Program
	{
		static void Main(string[] args)
		{
			var parser = new ArgParser();

			parser.Parse(args);
		}
	}
}