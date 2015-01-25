using System;

namespace Twirc.Lib
{
	public struct LoginResponse
	{
		public string Code;
		public string Message;
		public bool Success;

		public static readonly LoginResponse Successful = new LoginResponse
		{
			Code    = "0",
			Message = "",
			Success = true
		};

		public LoginResponse(bool success, string code, string message)
		{
			Code    = code;
			Message = message;
			Success = success;
		}
	}
}