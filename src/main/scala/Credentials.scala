package hubcat

import dispatch.Req

sealed trait Credentials {
  def sign(req: Req): Req
}

object Credentials {
  object None extends Credentials {
    def sign(req: Req) = req
  }

  case class OAuth(token: String)
    extends Credentials {
    def sign(req: Req) =
      req <:< Map("Authorization" -> s"token $token")
    }

  case class Basic(
    user: String,
    pass: String,
    otp: Option[String])
    extends Credentials {
    def sign(req: Req) =
      req.as_!(user, pass) <:< Map.empty[String, String] ++ otp.map("X-GitHub-OTP" -> _)
  }
}


