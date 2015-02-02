package hubcat

import org.json4s.JsonDSL._

/** Interfaces for issuing requests to create and access authorizations */
trait Authorizations { self: Requests =>

  /** Builder interface for new authorizations */
  protected [this]
  case class AuthorizationBuilder(
    _scopes: Option[Seq[String]]      = None,
    _note: Option[String]             = None,
    _url: Option[String]              = None, 
    _client: Option[(String, String)] = None)
    extends Client.Completion[Authorization] {

    def scopes(s: String*) = copy(_scopes = Some(s))
    def note(n: String) = copy(_note = Some(n))
    def url(u: String) = copy(_url = Some(u))
    def client(id: String, secret: String) = copy(
      _client = Some((id, secret))
    )

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations"
              << body)(handler)

    def body = json.str(
      ("scopes"        -> _scopes.map(_.toList).getOrElse(Nil)) ~
      ("note"          -> _note) ~
      ("note_url"      -> _url) ~
      ("client_id"     -> _client.map(_._1)) ~
      ("client_secret" -> _client.map(_._2)))
  }

  /** Builder for updating existing authorizations */
  protected [this]
  case class ReauthorizeBuilder(
    id: String,
    _scopes: Option[Seq[String]] = None,
    _scopeop: Option[Boolean]    = None,
    _url: Option[String]         = None,
    _note: Option[String]        = None)
    extends Client.Completion[Authorization] {

    def note(n: String) = copy(_note = Some(n))
    def url(u: String) = copy(_url = Some(u))
    def addScopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = Some(true))
    def removeScopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = Some(false))
    def scopes(sx: String*) =
      copy(_scopes = Some(sx), _scopeop = None)

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" / id
              << body)(handler)
    def body = json.str(
      ("note"     -> _note) ~
      ("note_url" -> _url) ~
      (_scopeop match {
        case Some(op) =>
          if (op) ("add_scopes" -> _scopes.map(_.toList))
          else ("remove_scopes" -> _scopes.map(_.toList))
            case _ => ("scopes" -> _scopes.map(_.toList))
      }))
  }

  /** fetch authorizations (http://developer.github.com/v3/oauth/#list-your-authorizations) */
  def authorizations =
    complete[List[Authorization]](apiHost / "authorizations")

  /** fetch one authorization (http://developer.github.com/v3/oauth/#get-a-single-authorization) */
  def authorization(id: Int) =
    complete[Authorization](apiHost / "authorizations" / id.toString)

  /** create a new authorization (http://developer.github.com/v3/oauth/#create-a-new-authorization) */
  def authorize =
    AuthorizationBuilder()

  /** update an existing authorization (http://developer.github.com/v3/oauth/#update-an-existing-authorization) */
  def reauthorize(id: String) =
    ReauthorizeBuilder(id)

  /** undo an authorization (http://developer.github.com/v3/oauth/#delete-an-authorization) */
  def deauthorize(id: Int) =
    complete[Unit](apiHost.DELETE / "authorizations" / id.toString)
}
