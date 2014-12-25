﻿using System;
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
		public delegate void LoginDel(LoginResponse response, string username, string password);
		public delegate void JoinDel(string channel);
		public delegate void MessageDel(string channel, string user, string message);
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
		/// Called when a server ping is received.
		/// </summary>
		public event Action OnPing = delegate {};

		/// <summary>
		/// Called when a message is sent from a channel.
		/// </summary>
		public event MessageDel OnMessage = delegate {};

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
		LoginResponse VerifyLogin()
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
					{
						return new LoginResponse
						{
							Status  = LoginStatus.Success,
							Code    = code,
							Message = line.Range(" :", "\r")
						};
					}
					if(code[0] == '5' || code[0] == '4' || code == "NOTICE")
					{
						return new LoginResponse
						{
							Status  = LoginStatus.Failed,
							Code    = code,
							Message = line.Range(" :", "\r")
						};
					}
				}
			}

			return new LoginResponse
			{
				Status  = LoginStatus.Failed,
				Code    = "-1",
				Message = "Unknown error"
			};
		}

		/// <summary>
		/// Processes a special line (ex: a line prefixed with :jtv) from the server.
		/// </summary>
		/// <param name="code">Message code.</param>
		/// <param name="line">Line to process.</param>
		void ProcessSpecialLine(string code, string line)
		{
			// TODO: Implement
		}

		/// <summary>
		/// Processes a standard line (ex: a message sent by someone) from the server.
		/// </summary>
		/// <param name="code">Message code.</param>
		/// <param name="line">Line to process.</param>
		void ProcessLine(string code, string line)
		{
			// Sending PONG after a PING request keeps the connection alive.
			if(line.StartsWith("PING"))
			{
				SendLine("PONG " + line.Substring("POING".Length));
				OnPing.Invoke();

				return;
			}

			string username = line.Range(":", "!");
			string channel  = line.Range("#", " ");

			if(String.IsNullOrEmpty(username) || String.IsNullOrEmpty(channel))
			{
				#if DEBUG
				// TODO: Replace with log class
				Console.WriteLine("Received empty line with code: " + code);
				#endif

				return;
			}

			switch(code)
			{
				case "PRIVMSG":
					OnMessage.Invoke(channel, username, line.Substring(line.IndexOf(':', 1) + 1));
					break;
			}
		}

		/// <summary>
		/// Sends a line-terminated UTF-8 string to the server.
		/// Automatically appends a line terminator to the string.
		/// </summary>
		/// <param name="data">Data to send.</param>
		void SendLine(string data)
		{
			//if(!Alive)
			//	return;

			Socket.Send(Encoding.UTF8.GetBytes(data + '\n'));
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
		public LoginResponse Login(string username, string password)
		{
			if(!Alive)
				return new LoginResponse(LoginStatus.NotConnected, "-1", "Not connected to server");

			_channels.Clear();

			SendLine("PASS " + password);
			SendLine("NICK " + username);
			SendLine("USER " + username + " 8 * :" + username);

			var loginInfo = VerifyLogin();

			Username = username;
			LoggedIn = loginInfo.Status == LoginStatus.Success;

			OnLogin.Invoke(loginInfo, username, password);

			return loginInfo;
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

				if(line.StartsWith(":jtv "))
					ProcessSpecialLine(code, line);
				else
					ProcessLine(code, line);
			}
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

	public struct LoginResponse
	{
		public LoginStatus Status;
		public string Code;
		public string Message;

		public static readonly LoginResponse Default = new LoginResponse
		{
			Status  = LoginStatus.Failed,
			Code    = "0",
			Message = ""
		};

		public LoginResponse(LoginStatus status, string code, string message)
		{
			Status  = status;
			Code    = code;
			Message = message;
		}
	}
}