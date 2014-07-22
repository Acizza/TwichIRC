using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace TwitchIRC
{
	public class TwitchHandler : AClientHandler
	{
		public Dictionary<string, UserData> UserStats { get; private set; }

		public TwitchHandler()
		{
			UserStats = new Dictionary<string, UserData>();
		}

		public override void OnConnect(string user, string host, int port)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkGreen, "Connected as " + user);
		}

		public override void OnJoin(string user, string channel)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkMagenta, user + " joined " + channel);
		}

		public override void OnMessage(string user, string channel, string msg)
		{
			if(msg.StartsWith("HISTORYEND ") || msg.StartsWith("USERCOLOR ") || msg.StartsWith("SPECIALUSER "))
				return;

			if(UserStats.ContainsKey(user))
			{
				var stats = UserStats[user];

				++stats.MessageCount;
				float sum = 0;

				foreach(var time in stats.MessageTimes)
					sum += (float)time.TotalSeconds;

				stats.AvgMessages = sum / stats.MessageTimes.Count;
				stats.AddMessageTime(DateTime.Now - stats.LastMsgTime);
				stats.LastMsgTime = DateTime.Now;
			}

			ConsoleUtil.Write(ConsoleColor.DarkYellow, "<" + channel + "> ");
			ConsoleUtil.Write(ConsoleColor.DarkRed, user);
			ConsoleUtil.WriteLine(ConsoleColor.White, ": " + msg);
		}

		public override void OnLeave(string user, string channel)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkRed, user + " left " + channel);
		}

		public override void OnDisconnect(string user)
		{

		}

		public override void OnUnknown(string line)
		{
			#if DEBUG
			Console.WriteLine("UNKNOWN: " + line);
			#endif
		}

		[Command("exit", 0, "", "Exits.")]
		public void ExitCommand(string[] args)
		{
			Environment.Exit(0);
		}

		[Command("close", 0, "", "Exits.")]
		public void CloseCommand(string[] args)
		{
			ExitCommand(null);
		}

		[Command("join", 1, "<channels>", "Joins channel. Multiple channels can be specified with spaces.")]
		public void JoinCommand(string[] args)
		{
			foreach(var arg in args)
				Program.Handler.Client.Join(arg);
		}

		[Command("channels", 0, "", "Lists all connected channels.")]
		public void ChannelsCommand(string[] args)
		{
			if(Program.Handler.Client.Channels.Count < 1)
			{
				Log.Warning("Not connected to any channels.");
				return;
			}

			uint index = 0;

			foreach(var channel in Program.Handler.Client.Channels)
			{
				++index;

				ConsoleUtil.Write(ConsoleColor.Red, index.ToString() + ". ");
				ConsoleUtil.WriteLine(ConsoleColor.Gray, channel);
			}
		}

		[Command("leave", 1, "<channels>", "Leaves channel. Multiple channels can be specified with spaces.")]
		public void LeaveCommand(string[] args)
		{
			foreach(var arg in args)
			{
				if(!Program.Handler.Client.Channels.Contains(arg))
				{
					Log.Warning("Not connected to channel: " + arg);
					continue;
				}

				Program.Handler.Client.Leave(arg);
			}
		}

		[Command("track", 1, "<users>", "Tracks a user for statistics. You can poll information by typing stats <name>.")]
		public void TrackCommand(string[] args)
		{
			foreach(var arg in args)
				UserStats.Add(arg, new UserData(30));
		}

		[Command("stats", 1, "<user>", "Grabs stats for a specific user.")]
		public void StatsCommand(string[] args)
		{
			var user = args[0];

			if(!UserStats.ContainsKey(user))
			{
				Log.Warning(user + " is not being tracked. Track them with track <name>.");
				return;
			}

			var stats   = UserStats[user];
			var builder = new StringBuilder();

			builder.Append("Messages: " + stats.MessageCount);
			builder.Append(string.Format("\nTime between messages: {0:N2} seconds", stats.AvgMessages));
			builder.Append(string.Format("\nLast message: {0:N2} seconds", stats.MessageTimes.Last().TotalSeconds));

			ConsoleUtil.WriteLine(ConsoleColor.DarkMagenta, builder.ToString());
		}
	}

	public class UserData
	{
		public uint     MessageCount;
		public List<TimeSpan> MessageTimes { get; private set; }
		public float    AvgMessages;
		public DateTime LastMsgTime;

		public int MaxMessageAverages { get; private set; }

		public UserData(int maxMessageAverages)
		{
			MaxMessageAverages = maxMessageAverages;
			MessageTimes = new List<TimeSpan>(maxMessageAverages);

			LastMsgTime = DateTime.Now;
		}

		public void AddMessageTime(TimeSpan span)
		{
			MessageTimes.Add(span);

			if(MessageTimes.Count >= MaxMessageAverages)
				MessageTimes.RemoveAt(0);
		}
	}
}