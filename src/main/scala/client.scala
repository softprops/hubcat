package hubcat

import com.ning.http.client.AsyncHandler
import dispatch._, dispatch.Defaults._
import scala.concurrent.Future

object Client {
  type Handler[T] = AsyncHandler[T]
  val Agent = "Hubcat/%s" format BuildInfo.version
  trait Completion {
    def apply[T](handler: Client.Handler[T]): Future[T]
  }
}

abstract class Requests(credentials: Credentials, http: Http = Http)
  extends DefaultHosts {
  def request[T](req: Req)(handler: Client.Handler[T]): Future[T] =
    http(credentials.sign(req) <:< Map("User-Agent" -> Client.Agent) > handler)
  def complete(req: Req): Client.Completion = new Client.Completion {
    override def apply[T](handler: Client.Handler[T]) =
      request(req)(handler)
  }
}

case class Client(token: String, http: Http = Http)
   extends Requests(OAuth2(token), http)
      with Gists
      with Issues
      with Markdown
      with Searching
      with Repositories {
  override def toString() = "%s(%s)".format(getClass.getSimpleName, "*" * token.size)
}

/** Client used for obtaining oauth authorization */
case class AuthorizationClient(user: String, pass: String, http: Http = Http)
   extends Requests(BasicAuth(user, pass), http)
      with Authorizations {
  override def toString() = "%s(%s,%s)".format(getClass.getSimpleName, user,"*" * pass.size)
}

