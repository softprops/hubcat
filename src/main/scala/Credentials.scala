package hubcat

import dispatch.Req

sealed trait Credentials {
  def sign(req: Req): Req
}

object Credentials {
  case class OAuth(access: String)
    extends Credentials {
    def sign(req: Req) =
      req <:< Map("Authorization" -> "token %s".format(access))
    }

  case class Basic(user: String, pass: String)
    extends Credentials {
    def sign(req: Req) = req.as_!(user, pass)
  }
}


