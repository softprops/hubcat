package hubcat

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req }
import scala.concurrent.{ ExecutionContext, Future }
import org.json4s.JValue
import org.json4s.native.JsonMethods.{ compact, render }

object Client {
  type Handler[T] = AsyncHandler[T]

  val Agent = s"Hubcat/${BuildInfo.version}"

  abstract class Completion[T: Rep] {
    def apply[T]
      (handler: Client.Handler[T]): Future[T]
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f))
  }
}

abstract class Requests(
  credentials: Credentials, http: Http = Http)
 (implicit ec: ExecutionContext)
  extends DefaultHosts {

  object json {
    def str(js: JValue) = compact(render(js))
  }

  def request[T]
   (req: Req)
   (handler: Client.Handler[T]): Future[T] =
    http(credentials.sign(req) <:< Map("User-Agent" -> Client.Agent) > handler)

  def complete(req: Req): Client.Completion[Response] =
    new Client.Completion[Response] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}

case class Client(
  token: String, http: Http = Http)
 (implicit ec: ExecutionContext)
  extends Requests(Credentials.OAuth(token), http)
     with Gists
     with Issues
     with Markdown
     with Searching
     with Repositories {
  override def toString() =
    s"${getClass.getSimpleName}(${"*" * token.size})"
}

/** Client used for obtaining oauth authorization */
case class AuthorizationClient(
  user: String,
  pass: String,
  http: Http = Http)
 (implicit ec: ExecutionContext)
  extends Requests(Credentials.Basic(user, pass), http)
     with Authorizations {
  override def toString() =
    s"${getClass.getSimpleName}($user,${"*" * pass.size})"
}

