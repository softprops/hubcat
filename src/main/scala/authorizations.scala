package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Authorizations { self: Requests =>
  protected [this]
  case class AuthorizationBuilder(scopesval: Option[Seq[String]] = None,
                                  noteval: Option[String] = None,
                                  urlval: Option[String] = None, 
                                  clientval: Option[(String, String)] = None)
     extends Client.Completion
        with Jsonizing {

    def scopes(s: String*) = copy(scopesval = Some(s))
    def note(n: String) = copy(noteval = Some(n))
    def url(u: String) = copy(urlval = Some(u))
    def client(id: String, secret: String) = copy(clientval = Some((id, secret)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val base: JObject = 
        ("scopes" -> scopesval.map(_.toList).getOrElse(Nil)) ~
        ("note" -> jStringOrNone(noteval)) ~
        ("note_url" -> jStringOrNone(urlval))
      val js = clientval.map {
        case (id, sec) => base ~ ("client_id" -> id) ~ ("client_secret" -> sec)
      }.getOrElse(base)
      compact(render(js))
    }
  }

  protected [this]
  case class ReauthorizeBuilder(id: String,
                                scopesval: Option[Seq[String]] = None,
                                scopeop: Option[Boolean] = None,
                                urlval: Option[String] = None,
                                noteval: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def note(n: String) = copy(noteval = Some(n))
    def url(u: String) = copy(urlval = Some(u))
    def addScopes(sx: String*) =
      copy(scopesval = Some(sx), scopeop = Some(true))
    def removeScopes(sx: String*) =
      copy(scopesval = Some(sx), scopeop = Some(false))
    def scopes(sx: String*) =
      copy(scopesval = Some(sx), scopeop = None)

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" / id << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val note =
        ("note" -> jStringOrNone(noteval)) ~
        ("note_url" -> jStringOrNone(urlval))
      val js = scopeop.map {
        op =>
          val scps = if (op) ("add_scopes" -> scopesval.map(_.toList))
          else ("remove_scopes" -> scopesval.map(_.toList))
          note ~ scps
      }.getOrElse(note ~ ("scopes" -> scopesval.map(_.toList)))

      compact(render(js))
    } 
  }

  /** fetch authorizations (http://developer.github.com/v3/oauth/#list-your-authorizations) */
  def authorizations =
    complete(apiHost / "authorizations")

  /** fetch one authorization (http://developer.github.com/v3/oauth/#get-a-single-authorization) */
  def authorization(id: String) =
    complete(apiHost / "authorizations" / id)

  /** create a new authorization (http://developer.github.com/v3/oauth/#create-a-new-authorization) */
  def authorize =
    AuthorizationBuilder()

  /** update an existing authorization (http://developer.github.com/v3/oauth/#update-an-existing-authorization) */
  def reauthorize(id: String) =
    ReauthorizeBuilder(id)

  /** undo an authorization (http://developer.github.com/v3/oauth/#delete-an-authorization) */
  def deauthorize(id: String) =
    complete(apiHost.DELETE / "authorizations" / id)
}
