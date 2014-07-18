using System;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace TwitchIRC
{
	public class IrcClient
	{
		public Socket Socket { get; private set; }
		public List<string> Channels { get; private set; }
		public AClientHandler ClientHandler;

		string user;

		public bool Alive { get { return Socket != null && Socket.Connected; } }
		public string User { get { return user; } }

		public IrcClient(AClientHandler handler)
		{
			Channels = new List<string>();

			ClientHandler = handler;
			ClientHandler.Client = this;
			ClientHandler.PostInit();
		}

		public IrcClient(AClientHandler handler, string host, int port, string user, string pass)
			: this(handler)
		{
			Connect(host, port, user, pass);
		}

		public void Connect(string host, int port, string user, string pass)
		{
			Close();

			Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			Socket.Connect(host, port);

			SendLine("PASS " + pass);
			SendLine("NICK " + user);
			SendLine("USER " + user + " 8 * :Bot");

			if(!ConfirmConnection())
			{
				Log.Error("Failed to connect to " + host + ":" + port.ToString());
				Shutdown();

				return;
			}

			this.user = user;

			ClientHandler.OnConnect(user, host, port);

			new Thread(Update).Start();
		}

		public bool ConfirmConnection()
		{
			string data;

			while((data = ReadLine()) != null)
			{
				foreach(var line in data.Split('\n'))
				{
					string code = line.Range(" ", " ");

					if(code == null || code.Length < 1)
						continue;

					if(code == "004" || code == "375")
						return true;
					if(code[0] == '5' || code[0] == '4' || code == "NOTICE")
						return false;
				}
			}

			return false;
		}

		void Update()
		{
			while(Alive)
			{
				string data;

				while((data = ReadLine()) != null)
				{
					foreach(var line in data.Split('\n'))
						ProcessLine(line + '\n');
				}
			}
		}

		void ProcessLine(string line)
		{
			if(line.StartsWith("PING "))
			{
				SendLine("PONG " + line.Substring("PING ".Length));
				return;
			}

			string type = line.Range(" ", " ");
			string user = line.Range(":", "!");
			string channel = line.Range("#", new [] { " ", "\n" });

			switch(type)
			{
				case "PRIVMSG":
					ClientHandler.OnMessage(user, channel, line.Range(" :", "\n", 2));
					break;

				case "JOIN":
					ClientHandler.OnJoin(user, channel);
					break;

				case "PART":
					ClientHandler.OnLeave(user, channel);
					break;

				default:
					ClientHandler.OnUnknown(line);
					break;
			}
		}

		public string ReadLine()
		{
			byte[] buffer = new byte[512];
			Socket.Receive(buffer);

			return Encoding.ASCII.GetString(buffer);
		}

		public void SendLine(string line)
		{
			Socket.Send(Encoding.ASCII.GetBytes(line + '\n'));
		}

		public void Join(string channel)
		{
			Channels.Add(channel);
			SendLine("JOIN #" + channel);
		}

		public void Disconnect()
		{
			foreach(var channel in Channels)
				SendLine("PART #" + channel);

			SendLine("QUIT :Bot disconnect");
		}

		public void Shutdown()
		{
			Socket.Shutdown(SocketShutdown.Both);
			Socket.Close();
		}

		public void Close()
		{
			if(Alive)
			{
				Disconnect();
				Shutdown();
			}
		}
	}
}