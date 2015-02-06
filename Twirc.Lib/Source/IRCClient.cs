using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using Twirc.Lib.Util;

namespace Twirc.Lib
{
	public sealed class IRCClient : IDisposable
	{
		/// <summary>
		/// The socket used for sending and receiving data from the server.
		/// </summary>
		/// <value>The socket.</value>
		public Socket Socket { get; private set; }

		public string Username { get; private set; }
		public bool LoggedIn   { get; private set; }

		/// <summary>
		/// The amount of bytes currently being read from the server.
		/// </summary>
		/// <value>The size of the max buffer.</value>
		public uint CurrentBufferSize { get; private set; }

		/// <summary>
		/// Returns true if the server information (host, port) has been changed since the last connection.
		/// </summary>
		/// <value><c>true</c> if the server information changed; otherwise, <c>false</c>.</value>
		public bool ConnectInfoDirty { get; private set; }

		/// <summary>
		/// The maximum amount of bytes to read from the server for the join user list.
		/// </summary>
		public uint MaxJoinBufferSize = 64 * 1024;

		/// <summary>
		/// The maximum amount of bytes to read from the server for messages.
		/// </summary>
		public uint MaxMessageBufferSize = 512;

		public delegate void ConnectDel(string host, int port);
		public delegate void LoginDel(LoginResponse response, string username);
		public delegate void JoinDel(Channel channel, string username);
		public delegate void MessageDel(Channel channel, string username, string message);
		public delegate void UserSubscribedDel(Channel channel, string username);
		public delegate void LeaveDel(Channel channel, string username);
		public delegate void LogoutDel(string username);

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
		public event LogoutDel OnLogout = delegate {};

		public string Host
		{
			get
			{
				return _host;
			}
			set
			{
				if(_host != value)
					ConnectInfoDirty = true;

				_host = value;
			}
		}

		public int Port
		{
			get
			{
				return _port;
			}
			set
			{
				if(_port != value)
					ConnectInfoDirty = true;

				_port = value;
			}
		}

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
		private string _host;
		private int _port;

		public IRCClient()
		{
			_channels = new List<Channel>();
			CurrentBufferSize = MaxMessageBufferSize;
		}

		public IRCClient(string host, int port) : this()
		{
			Connect(host, port);
		}

		public IRCClient(string host, int port, string username, string password) : this()
		{
			ConnectAndLogin(host, port, username, password);
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

			Close();
		}

		/// <summary>
		/// Connects to the server specified by the Host and Port fields.
		/// </summary>
		public void Connect()
		{
			Connect(Host, Port);
		}

		/// <summary>
		/// Connects to the specified server.
		/// </summary>
		/// <param name="host">Host.</param>
		/// <param name="port">Port.</param>
		public void Connect(string host, int port)
		{
			if(Alive)
				Close();

			Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			Socket.Connect(host, port);

			_host = host;
			_port = port;
			ConnectInfoDirty = false;

			OnConnect.Invoke(host, port);
		}

		/// <summary>
		/// Connects to the specified server and attempts to login with the specified user information.
		/// </summary>
		/// <param name="host">Host.</param>
		/// <param name="port">Port.</param>
		/// <param name="username">Username.</param>
		/// <param name="password">Password.</param>
		public void ConnectAndLogin(string host, int port, string username, string password)
		{
			Connect(host, port);
			Login(username, password);
		}

		/// <summary>
		/// Reconnects using the same host and port if the host address isn't empty.
		/// </summary>
		public void Reconnect()
		{
			if(String.IsNullOrEmpty(Host))
				return;

			Connect(Host, Port);
		}

		/// <summary>
		/// Sends a login request with the specified username and password.
		/// </summary>
		/// <param name="username">Username.</param>
		/// <param name="password">Password.</param>
		public void Login(string username, string password)
		{
			if(!Alive)
				throw new Exception("Must be connected to login.");

			if(LoggedIn)
				Connect(Host, Port);

			_channels.Clear();

			Username = username;
			LoggedIn = false;

			SendLine("PASS " + password);
			SendLine("NICK " + username);
			SendLine("USER " + username + " 8 * :" + username);
		}

		/// <summary>
		/// Logs out from the server.
		/// </summary>
		public void Logout()
		{
			if(!LoggedIn)
				throw new Exception("Must be logged in to logout.");

			LoggedIn = false;
			SendLine("QUIT :Logout");

			OnLogout.Invoke(Username);
			_channels.Clear();
		}

		/// <summary>
		/// Joins the specified channel.
		/// </summary>
		/// <param name="channel">Channel name.</param>
		public void Join(string channel)
		{
			CurrentBufferSize = MaxJoinBufferSize;
			SendLine("JOIN #" + channel);
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

		public Channel GetChannelByName(string channel)
		{
			return _channels.Find(x => x.Name == channel);
		}

		/// <summary>
		/// Leaves the specified channel.
		/// </summary>
		/// <param name="channelName">Channel.</param>
		public void Leave(string channelName)
		{
			var channel = GetChannelByName(channelName);

			SendLine("PART #" + channelName);
			_channels.Remove(channel);

			OnLeave.Invoke(channel, Username);
		}

		/// <summary>
		/// Sends a chat message to the specified channel.
		/// </summary>
		/// <returns><c>true</c>, if message was sent, <c>false</c> otherwise.</returns>
		/// <param name="channelName">Channel name.</param>
		/// <param name="message">Message.</param>
		public bool SendMessage(string channelName, string message)
		{
			if(!IsConnectedTo(channelName))
				return false;

			SendLine("PRIVMSG #" + channelName + " :" + message);
			return true;
		}

		/// <summary>
		/// Reads a UTF-8 string from the server.
		/// </summary>
		/// <returns>The line.</returns>
		public string ReadLine()
		{
			if(!Alive)
				return "";

			var buffer = new byte[CurrentBufferSize];
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
			if(!Alive)
				return;

			string data = ReadLine();

			// If no data is received, the server closed the connection.
			if(data.Length == 0)
			{
				Close();
				return;
			}

			foreach(var line in data.Split('\n'))
			{
				if(line.Length == 0)
					continue;

				string code = line.Range(" ", " ");

				if(code.Length > 0)
					ProcessCode(code, line);

				if(line.StartsWith(":jtv "))
					ProcessSpecialLine(code, line);
				else
					ProcessLine(code, line);
			}
		}

		private void ProcessCode(string code, string line)
		{
			switch(code)
			{
				// TODO: Write a class to handle joins. Incomplete names are sometimes sent as a second message.
				case "353":
					var channelName = line.Range("#", 0, " ", "\r");
					var channel     = GetChannelByName(channelName);

					if(channel == null)
					{
						channel = new Channel(channelName);
						_channels.Add(channel);

						OnJoin.Invoke(channel, Username);
					}

					ParseChannelViewers(channel, line);
					break;

				case "356": // End of join list, so restore the receive rate.
					CurrentBufferSize = MaxMessageBufferSize;
					break;

				case "004":
				case "375":
					if(LoggedIn)
						break;

					LoggedIn = true;
					OnLogin.Invoke(new LoginResponse(true, code, "Login successful."), Username);
					break;
			}

			if(code[0] == '5' || code[0] == '4' || code == "NOTICE")
				OnLogin.Invoke(new LoginResponse(false, code, line.From(" :")), Username);
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

			var channel = GetChannelByName(line.Range("#", 0, " ", "\r"));

			if(channel == null)
				return;

			string username = line.Range(":", "!");

			if(username.Length == 0)
				return;

			// Assume anything from twitchnotify is a subscription (for now).
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
					if(channel.Users.Contains(username))
						break;

					channel.Users.Add(username);

					// Only send the join for users other than ourself, so we can send the proper join with a list of viewers later.
					if(username != Username)
						OnJoin.Invoke(channel, username);

					break;

				case "PART":
					if(!channel.Users.Contains(username))
						break;

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
			foreach(var name in line.From(" :").Split(' '))
			{
				if(channel.Users.Contains(name))
					continue;

				channel.Users.Add(name);
			}
		}

		/// <summary>
		/// Shuts down and closes the socket.
		/// </summary>
		public void Close()
		{
			Socket.Shutdown(SocketShutdown.Both);
			Socket.Close();
		}
	}
}