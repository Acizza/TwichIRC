using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Net.Sockets;
using System.Text;
using Twirc.Lib.Util;

namespace Twirc.Lib
{
	public sealed class IRCClient : IDisposable
	{
		public delegate void ConnectDel(string host, int port);
		public delegate void LoginDel(string username, string password);
		public delegate void JoinDel(string channel);
		public delegate void LeaveDel(string channel);

		/// <summary>
		/// The socket used for sending and receiving data from the server.
		/// </summary>
		/// <value>The socket.</value>
		public Socket Socket { get; private set; }

		/// <summary>
		/// A value indicating if the client is logged in to the connected server.
		/// </summary>
		/// <value><c>true</c> if logged in; otherwise, <c>false</c>.</value>
		public bool LoggedIn { get; private set; }

		/// <summary>
		/// The username currently logged in.
		/// </summary>
		/// <value>The username.</value>
		public string Username { get; private set; }

		/// <summary>
		/// The maximum amount of bytes to read from the server.
		/// </summary>
		public uint MaxBufferSize = 512;

		/// <summary>
		/// Called when a successful connection occurs.
		/// </summary>
		public event ConnectDel OnConnect = delegate {};

		/// <summary>
		/// Called when a successful login occurs.
		/// </summary>
		public event LoginDel OnLogin = delegate {};

		/// <summary>
		/// Called when a channel is joined.
		/// </summary>
		public event JoinDel OnJoin = delegate {};

		/// <summary>
		/// Called when a channel is left.
		/// </summary>
		public event LeaveDel OnLeave = delegate {};

		/// <summary>
		/// Called when a logout is requested.
		/// </summary>
		public event Action OnLogout = delegate {};

		/// <summary>
		/// The host address being used. When set, <see cref="Connect"/> is called with the new information.
		/// </summary>
		/// <value>The host address.</value>
		public string Host
		{
			get
			{
				return _host;
			}
			set
			{
				_host = value;
				Connect(value, Port);
			}
		}

		/// <summary>
		/// The port being used. When set, <see cref="Connect"/> is called with the new information. 
		/// </summary>
		/// <value>The port.</value>
		public int Port
		{
			get
			{
				return _port;
			}
			set
			{
				_port = value;
				Connect(Host, value);
			}
		}

		/// <summary>
		/// Returns true if the socket being used is not null and is connected.
		/// </summary>
		/// <value><c>true</c> if connected; otherwise, <c>false</c>.</value>
		public bool Alive
		{
			get
			{
				return Socket != null && Socket.Connected;
			}
		}

		/// <summary>
		/// The list of currently connected channels.
		/// </summary>
		/// <value>The channels.</value>
		public ReadOnlyCollection<string> Channels
		{
			get
			{
				return _channels.AsReadOnly();
			}
		}

		string _host;
		int _port;
		List<string> _channels;

		public IRCClient()
		{
			_channels = new List<string>();
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
		/// Checks any codes sent by the server to see if a login attempt was successful.
		/// </summary>
		/// <returns><c>true</c>, if login was successful, <c>false</c> otherwise.</returns>
		bool VerifyLogin()
		{
			// TODO: Add an anti-blocking mechanism to avoid this hanging *forever* if no valid codes are received.
			string data;

			while((data = ReadLine()) != null)
			{
				foreach(var line in data.Split('\n'))
				{
					string code = line.Range(" ");

					if(code.Length == 0)
						continue;

					if(code == "004" || code == "375")
						return true;
					if(code[0] == '5' || code[0] == '4' || code == "NOTICE")
						return false;
				}
			}

			return false;
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

			_host = host;
			_port = port;

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
		public LoginStatus Login(string username, string password)
		{
			if(!Alive)
				return LoginStatus.NotConnected;

			_channels.Clear();

			Username = username;
			LoggedIn = false;

			SendLine("PASS " + password);
			SendLine("NICK " + username);
			SendLine("USER " + username + " 8 * :" + username);

			if(!VerifyLogin())
				return LoginStatus.Failed;

			LoggedIn = true;
			OnLogin.Invoke(username, password);

			return LoginStatus.Success;
		}

		/// <summary>
		/// Sends a line-terminated UTF-8 string to the server.
		/// Automatically appends a line terminator to the string.
		/// </summary>
		/// <param name="data">Data to send.</param>
		public void SendLine(string data)
		{
			if(!Alive)
				return;

			Socket.Send(Encoding.UTF8.GetBytes(data + '\n'));
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
		/// Joins the specified channel.
		/// </summary>
		/// <param name="channel">Channel name.</param>
		public void Join(string channel)
		{
			SendLine("JOIN #" + channel);

			OnJoin.Invoke(channel);
			_channels.Add(channel);
		}

		/// <summary>
		/// Leaves the specified channel.
		/// </summary>
		/// <param name="channel">Channel.</param>
		public void Leave(string channel)
		{
			SendLine("PART #" + channel);

			_channels.Remove(channel);
			OnLeave.Invoke(channel);
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
		/// Disconnects from the server and closes the socket.
		/// </summary>
		public void Dispose()
		{
			if(!Alive)
				return;

			Logout();

			Socket.Shutdown(SocketShutdown.Both);
			Socket.Close();
		}
	}

	public enum LoginStatus
	{
		Success,
		Failed,
		NotConnected
	}
}