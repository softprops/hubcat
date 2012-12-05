package hubcat

import dispatch._
import com.ning.http.client.{ RequestBuilder, Response }

object Client {
  type Handler[T] = (Response => T)
  trait Completion {
    def apply[T](handler: Client.Handler[T]): Promise[T]
  }
}

abstract class Requests(credentials: Credentials, http: Http = Http)
  extends DefaultHosts {
  def request[T](req: RequestBuilder)(handler: Client.Handler[T]): Promise[T] =
    http(credentials.sign(req) > handler)
  def complete(req: RequestBuilder) = new Client.Completion {
    override def apply[T](handler: Client.Handler[T]) =
      request(req)(handler)
  }
}

class Client(credentials: Credentials, http: Http = Http)
  extends Requests(credentials, http)
     with Gists
     with Git
     with Issues
     with Markdown
     with Searching
     with Repositories


class AuthorizationClient(user: String, pass: String, http: Http = Http)
  extends Requests(BasicAuth(user, pass), http)
     with Authorizations {
  override def toString() = "%s(%s,%s)".format(getClass.getSimpleName, user,"*"*pass.size)
}

