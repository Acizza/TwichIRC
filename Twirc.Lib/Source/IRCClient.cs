using System;
using System.Net.Sockets;

namespace Twirc.Lib
{
	public sealed class IRCClient
	{
		public enum LoginStatus
		{
			Success,
			Failed,
			NotConnected
		}

		/// <summary>
		/// The socket used for sending and receiving data from the server.
		/// </summary>
		/// <value>The socket.</value>
		public Socket Socket { get; private set; }

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

		string _host;
		int _port;

		/// <summary>
		/// Connects to the specified server.
		/// </summary>
		/// <param name="host">Host.</param>
		/// <param name="port">Port.</param>
		public void Connect(string host, int port)
		{
			Close();

			Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			Socket.Connect(host, port);

			_host = host;
			_port = port;
		}

		/// <summary>
		/// Login the specified username and password. Returns true if successful.
		/// </summary>
		/// <param name="username">Username.</param>
		/// <param name="password">Password.</param>
		public LoginStatus Login(string username, string password)
		{
			if(!Alive)
				return LoginStatus.NotConnected;

			// TODO: Finish

			return LoginStatus.Success;
		}

		public void Disconnect()
		{
			if(!Alive)
				return;

			// TODO: Finish
		}

		/// <summary>
		/// Disconnects from the server and closes the socket.
		/// </summary>
		public void Close()
		{
			if(!Alive)
				return;

			Disconnect();

			Socket.Shutdown(SocketShutdown.Both);
			Socket.Close();
		}
	}
}