package hubcat

import dispatch.Req

sealed trait Credentials {
  def sign(req: Req): Req
}

case class OAuth2(access: String) extends Credentials {
  def sign(req: Req) =
    req <:< Map("Authorization" -> "token %s".format(access))
}

case class BasicAuth(user: String, pass: String) extends Credentials {
  def sign(req: Req) =
    req.as_!(user, pass)
}
