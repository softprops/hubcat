package hubcat

import com.ning.http.client.{ AsyncHandler, Response }
import dispatch.{ FunctionHandler, Http, Req }
import scala.concurrent.{ ExecutionContext, Future }
import org.json4s.JValue
import org.json4s.native.JsonMethods.{ compact, render }

object Client {
  type Handler[T] = AsyncHandler[T]

  val Agent = s"Hubcat/${BuildInfo.version}"

  /** Completes requests with handlers */
  abstract class Completion[T: Rep] {

    /** handles request with a provided Client.Handler */
    def apply[T]
      (handler: Client.Handler[T]): Future[T]

    /** handles requests by mapping a respond to a default Rep */
    def apply(): Future[T] =
      apply(implicitly[Rep[T]].map)

    /** handles requests with a Response function handler */
    def apply[T]
      (f: Response => T): Future[T] =
        apply(new FunctionHandler(f))
  }

  case class TokenClient(
    token: String, http: Http)
   (implicit ec: ExecutionContext)
    extends Requests(
      Credentials.OAuth(token), http)
      with Gists
      with Issues
      with Markdown
      with Searching
      with Repositories {
   def close() = http.shutdown()
   override def toString() =
     s"${getClass.getSimpleName}(${"*" * token.size})"
  }

  /** Client used for obtaining oauth authorization */
  case class AuthorizationClient(
    user: String,
    pass: String,
    otp: Option[String],
    http: Http)
   (implicit ec: ExecutionContext)
    extends Requests(
      Credentials.Basic(user, pass, otp), http)
      with Authorizations {
    override def toString() =
      s"${getClass.getSimpleName}($user,${"*" * pass.size})"
  }

  /** Primary API interface. requires a valid oauth2 token */
  def apply(
    token: String, http: Http = new Http)
   (implicit ec: ExecutionContext) =
    TokenClient(token, http)

  /** Authorizations API interface. requires username & password. For members that use
   *  2-factor authorization an optional otp field is should be set to the 2-factor-authentication
   *  code sent by github */
  def authorizations(
    user: String, pass: String,
    otp: Option[String] = None, http: Http = new Http)
   (implicit ec: ExecutionContext) =
    AuthorizationClient(user, pass, otp, http)
}

abstract class Requests(
  credentials: Credentials, http: Http = new Http)
 (implicit ec: ExecutionContext)
  extends DefaultHosts {

  object json {
    def str(js: JValue) = compact(render(js))
  }

  def request[T]
   (req: Req)
   (handler: Client.Handler[T]): Future[T] =
    http(credentials.sign(req) <:< Map("User-Agent" -> Client.Agent) > handler)

  /** A simple completion handler for requests.
   *  Completes requests with a default Rep */
  def complete[A: Rep](req: Req): Client.Completion[A] =
    new Client.Completion[A] {
      override def apply[T](handler: Client.Handler[T]) =
        request(req)(handler)
    }
}
