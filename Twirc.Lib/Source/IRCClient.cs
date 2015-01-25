using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Net.Sockets;
using System.Text;
using Twirc.Lib.Util;
using System.Linq;

namespace Twirc.Lib
{
	public sealed class IRCClient : IDisposable
	{
		/// <summary>
		/// The socket used for sending and receiving data from the server.
		/// </summary>
		/// <value>The socket.</value>
		public Socket Socket { get; private set; }

		public bool LoggedIn   { get; private set; }
		public string Username { get; private set; }

		/// <summary>
		/// The maximum amount of bytes to read from the server.
		/// </summary>
		public uint MaxBufferSize = 512;

		public delegate void ConnectDel(string host, int port);
		public delegate void LoginDel(LoginResponse response, string username);
		public delegate void JoinDel(Channel channel, string username);
		public delegate void MessageDel(Channel channel, string username, string message);
		public delegate void UserSubscribedDel(Channel channel, string username);
		public delegate void LeaveDel(Channel channel, string username);

		/// <summary>
		/// Called when a successful connection occurs.
		/// </summary>
		public event ConnectDel OnConnect = delegate {};

		/// <summary>
		/// Called when a response to a login request is received.
		/// </summary>
		public event LoginDel OnLogin = delegate {};

		/// <summary>
		/// Called when a user joins a connected channel.
		/// </summary>
		public event JoinDel OnJoin = delegate {};

		/// <summary>
		/// Called when a server ping is received.
		/// </summary>
		public event Action OnPing = delegate {};

		/// <summary>
		/// Called when a message is sent from a channel.
		/// </summary>
		public event MessageDel OnMessage = delegate {};

		/// <summary>
		/// Called when a user subcribes to a connected channel.
		/// </summary>
		public event UserSubscribedDel OnUserSubscribed = delegate {};

		/// <summary>
		/// Called when a user leaves a connected channel.
		/// </summary>
		public event LeaveDel OnLeave = delegate {};

		/// <summary>
		/// Called when a logout is requested.
		/// </summary>
		public event Action OnLogout = delegate {};

		public string Host { get; private set; }
		public int Port    { get; private set; }

		/// <summary>
		/// Returns true if the socket being used is not null and is connected.
		/// </summary>
		/// <value><c>true</c> if connected; otherwise, <c>false</c>.</value>
		public bool Alive
		{
			get { return Socket != null && Socket.Connected; }
		}

		/// <summary>
		/// The list of currently connected channels.
		/// </summary>
		/// <value>The channels.</value>
		public ReadOnlyCollection<Channel> Channels
		{
			get { return _channels.AsReadOnly(); }
		}

		private List<Channel> _channels;

		public IRCClient()
		{
			_channels = new List<Channel>();
		}

		public IRCClient(string host, int port) : this()
		{
			Connect(host, port);
		}

		public IRCClient(string host, int port, string username, string password) : this()
		{
			Connect(host, port, username, password);
		}

		~IRCClient()
		{
			Dispose();
		}

		/// <summary>
		/// Disconnects from the server and closes the socket.
		/// </summary>
		public void Dispose()
		{
			if(!Alive)
				return;

			Socket.Shutdown(SocketShutdown.Both);
			Socket.Close();
		}

		/// <summary>
		/// Connects to the specified server.
		/// </summary>
		/// <param name="host">Host.</param>
		/// <param name="port">Port.</param>
		public void Connect(string host, int port)
		{
			Dispose();

			Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			Socket.Connect(host, port);

			Host = host;
			Port = port;

			OnConnect.Invoke(host, port);
		}

		/// <summary>
		/// Connects to the specified server and attempts to login with the specified user information.
		/// </summary>
		/// <param name="host">Host.</param>
		/// <param name="port">Port.</param>
		/// <param name="username">Username.</param>
		/// <param name="password">Password.</param>
		public void Connect(string host, int port, string username, string password)
		{
			Connect(host, port);
			Login(username, password);
		}

		/// <summary>
		/// Attempt to login with the specified username and password.
		/// </summary>
		/// <param name="username">Username.</param>
		/// <param name="password">Password.</param>
		public void Login(string username, string password)
		{
			if(!Alive)
				return;

			if(LoggedIn)
				Connect(Host, Port);

			_channels.Clear();

			SendLine("PASS " + password);
			SendLine("NICK " + username);
			SendLine("USER " + username + " 8 * :" + username);

			Username = username;
			LoggedIn = false;
		}

		/// <summary>
		/// Logs out from the server.
		/// </summary>
		public void Logout()
		{
			if(!Alive || !LoggedIn)
				return;

			SendLine("QUIT :Logout");

			OnLogout.Invoke();
			_channels.Clear();
		}

		/// <summary>
		/// Joins the specified channel.
		/// </summary>
		/// <param name="channel">Channel name.</param>
		public void Join(string channel)
		{
			SendLine("JOIN #" + channel);
			_channels.Add(new Channel(channel));
		}

		/// <summary>
		/// Returns true if the client is currently connected to the specified channel.
		/// </summary>
		/// <returns><c>true</c> if this instance is connected to the specified channel; otherwise, <c>false</c>.</returns>
		/// <param name="channel">Channel.</param>
		public bool IsConnectedTo(string channel)
		{
			return _channels.Any(x => x.Name == channel);
		}

		/// <summary>
		/// Leaves the specified channel.
		/// </summary>
		/// <param name="channelName">Channel.</param>
		public void Leave(string channelName)
		{
			var channel = _channels.Find(x => x.Name == channelName);

			SendLine("PART #" + channelName);
			_channels.Remove(channel);

			OnLeave.Invoke(channel, Username);
		}

		/// <summary>
		/// Reads a UTF-8 string from the server.
		/// </summary>
		/// <returns>The line.</returns>
		public string ReadLine()
		{
			if(!Alive)
				return "";

			var buffer = new byte[MaxBufferSize];
			int read   = Socket.Receive(buffer);

			return Encoding.UTF8.GetString(buffer, 0, read);
		}

		/// <summary>
		/// Sends a line-terminated UTF-8 string to the server.
		/// Automatically appends a line terminator to the string.
		/// </summary>
		/// <param name="data">Data to send.</param>
		private void SendLine(string data)
		{
			Socket.Send(Encoding.UTF8.GetBytes(data + "\n"));
		}

		/// <summary>
		/// Reads and processes the next line from the server.
		/// </summary>
		public void ProcessNextLine()
		{
			string data = ReadLine();

			foreach(var line in data.Split('\n'))
			{
				if(line.Length == 0)
					continue;

				string code = line.Range(" ", " ");

				ProcessCode(code, line);

				if(line.StartsWith(":jtv "))
					ProcessSpecialLine(code, line);
				else
					ProcessLine(code, line);
			}
		}

		private void ProcessCode(string code, string line)
		{
			if(!LoggedIn)
			{
				var channel = _channels.Find(x => x.Name == line.From("#"));

				switch(code)
				{
					case "353":
						// TODO: Make list of channels available when a channel is joined
						ParseChannelViewers(channel, line);
						break;

					case "004":
					case "375":
						LoggedIn = true;
						OnLogin.Invoke(new LoginResponse(true, code, "Login successful."), Username);
						break;
				}

				if(code[0] == '5' || code[0] == '4' || code == "NOTICE")
					OnLogin.Invoke(new LoginResponse(false, code, line.From(" :")), Username);
			}
		}

		/// <summary>
		/// Processes a special line (ex: a line prefixed with :jtv) from the server.
		/// </summary>
		/// <param name="code">Message code.</param>
		/// <param name="line">Line to process.</param>
		private void ProcessSpecialLine(string code, string line)
		{
			// TODO: Implement
		}

		// TODO: Refactor
		/// <summary>
		/// Processes a standard line (ex: a message sent by someone) from the server.
		/// </summary>
		/// <param name="code">Message code.</param>
		/// <param name="line">Line to process.</param>
		private void ProcessLine(string code, string line)
		{
			// Sending PONG after a PING request keeps the connection alive.
			if(line.StartsWith("PING"))
			{
				SendLine(line.From("PONG ", false));
				OnPing.Invoke();

				return;
			}

			var channel = _channels.Find(x => x.Name == line.Range("#", 0, " ", "\r"));

			if(channel == null)
				return;

			string username = line.Range(":", "!");

			if(username.Length == 0)
				return;

			// Assume anything from twitchnotify is a subscription (for now)
			if(username == "twitchnotify")
			{
				OnUserSubscribed.Invoke(channel, line.Range(":", " ", 1));
				return;
			}

			switch(code)
			{
				case "PRIVMSG":
					OnMessage.Invoke(channel, username, line.From(":", true, 1));
					break;

				case "JOIN":
					channel.Users.Add(username);
					OnJoin.Invoke(channel, username);
					break;

				case "PART":
					channel.Users.Remove(username);
					OnLeave.Invoke(channel, username);
					break;
			}
		}

		/// <summary>
		/// Parses a message containing the list of viewers and adds them to the channel's user list.
		/// </summary>
		/// <param name="channel">Channel to add the users to.</param>
		/// <param name="line">Line of data to parse.</param>
		private static void ParseChannelViewers(Channel channel, string line)
		{
			foreach(var name in line.To(" :").Split(' '))
				channel.Users.Add(name);
		}
	}
}