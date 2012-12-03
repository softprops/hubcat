package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Authorizations { self: Client =>
  case class AuthorizationBuilder(scopesval: Option[Seq[String]] = None,
                                  noteval: Option[String] = None,
                                  urlval: Option[String] = None, 
                                  clientval: Option[(String, String)] = None)
     extends Client.Completion {
    def scopes(s: String*) = copy(scopesval = Some(s))
    def note(n: String) = copy(noteval = Some(n))
    def url(u: String) = copy(urlval = Some(u))
    def client(id: String, secret: String) = copy(clientval = Some((id, secret)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val base: JObject = 
        ("scopes" -> scopesval.map(_.toList).getOrElse(Nil)) ~
        ("note" -> noteval.map(JString(_)).getOrElse(JNothing)) ~
        ("note_url" -> urlval.map(JString(_)).getOrElse(JNothing))
      val js = clientval.map {
        case (id, sec) => base ~ ("client_id" -> id) ~ ("client_secret" -> sec)
      }.getOrElse(base)
      compact(render(js))
    }
  }

  case class ReauthorizeBuilder(id: String,
                                scopesval: Option[Seq[String]] = None,
                                scopeop: Option[Boolean] = None,
                                urlval: Option[String] = None,
                                noteval: Option[String] = None)
     extends Client.Completion {
    def note(n: String) = copy(noteval = Some(n))
    def url(u: String) = copy(urlval = Some(u))
    def addScopes(sx: Seq[String]) =
      copy(scopesval = Some(sx), scopeop = Some(true))
    def removeScopes(sx: Seq[String]) =
      copy(scopesval = Some(sx), scopeop = Some(false))
    def scopes(sx: String*) =
      copy(scopesval = Some(sx), scopeop = None)

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "authorizations" / id << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val note =
        ("note" -> noteval.map(JString(_)).getOrElse(JNothing)) ~
        ("note_url" -> urlval.map(JString(_)).getOrElse(JNothing))
      val js = scopeop.map {
        op =>
          val scps = if (op) ("add_scopes" -> scopesval.map(_.toList))
          else ("remove_scopes" -> scopesval.map(_.toList))
          note ~ scps
      }.getOrElse(note ~ ("scopes" -> scopesval.map(_.toList)))

      compact(render(js))
    } 
  }

  def authorizations =
    complete(apiHost / "authorizations")

  def authorization(id: String) =
    complete(apiHost / "authorizations" / id)

  def authorize = AuthorizationBuilder()

  def reauthorize(id: String) =
    ReauthorizeBuilder(id)

  def deauthorize(id: String) =
    complete(apiHost.DELETE / "authorizations" / id)
}
