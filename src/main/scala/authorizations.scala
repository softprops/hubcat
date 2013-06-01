package hubcat

import com.ning.http.client.RequestBuilder
import dispatch._
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

trait Authorizations { self: Requests =>
  /** Builder for new authorizations */
  protected [this]
  case class AuthorizationBuilder(
    _scopes: Option[Seq[String]] = None,
    _note: Option[String] = None,
    _url: Option[String] = None, 
    _client: Option[(String, String)] = None)
     extends Client.Completion
        with Jsonizing {

    def scopes(s: String*) = copy(_scopes = Some(s))
    def note(n: String) = copy(_note = Some(n))
    def url(u: String) = copy(_url = Some(u))
    def client(id: String, secret: String) = copy(_client = Some((id, secret)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" << pjson)(handler)

    private def pjson = {
      val base = 
        ("scopes" -> _scopes.map(_.toList).getOrElse(Nil)) ~
        ("note" -> jStringOrNone(_note)) ~
        ("note_url" -> jStringOrNone(_url))
      val js = _client.map {
        case (id, sec) => base ~ ("client_id" -> id) ~ ("client_secret" -> sec)
      }.getOrElse(base)
      compact(render(js))
    }
  }

  /** Builder for updating existing authorizations */
  protected [this]
  case class ReauthorizeBuilder(
    id: String,
    _scopes: Option[Seq[String]] = None,
    _scopeop: Option[Boolean] = None,
    _url: Option[String] = None,
    _note: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def note(n: String) = copy(_note = Some(n))
    def url(u: String) = copy(_url = Some(u))
    def addScopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = Some(true))
    def removeScopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = Some(false))
    def scopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = None)

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" / id << pjson)(handler)

    private def pjson = {
      val note =
        ("note" -> jStringOrNone(_note)) ~
        ("note_url" -> jStringOrNone(_url))
      val js = _scopeop.map {
        op =>
          val scps = if (op) ("add_scopes" -> _scopes.map(_.toList))
          else ("remove_scopes" -> _scopes.map(_.toList))
          note ~ scps
      }.getOrElse(note ~ ("scopes" -> _scopes.map(_.toList)))

      compact(render(js))
    } 
  }

  /** fetch authorizations (http://developer.github.com/v3/oauth/#list-your-authorizations) */
  def authorizations =
    complete(apiHost / "authorizations")

  /** fetch one authorization (http://developer.github.com/v3/oauth/#get-a-single-authorization) */
  def authorization(id: Int) =
    complete(apiHost / "authorizations" / id.toString)

  /** create a new authorization (http://developer.github.com/v3/oauth/#create-a-new-authorization) */
  def authorize =
    AuthorizationBuilder()

  /** update an existing authorization (http://developer.github.com/v3/oauth/#update-an-existing-authorization) */
  def reauthorize(id: String) =
    ReauthorizeBuilder(id)

  /** undo an authorization (http://developer.github.com/v3/oauth/#delete-an-authorization) */
  def deauthorize(id: Int) =
    complete(apiHost.DELETE / "authorizations" / id.toString)
}
