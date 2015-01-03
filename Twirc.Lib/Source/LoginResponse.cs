using System;

namespace Twirc.Lib
{
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