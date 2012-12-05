package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Gists { self: Requests =>
  case class GistBuilder(filevals: Map[String, String] = Map.empty[String, String],
                         descval: Option[String] = None,
                         vis: Boolean = true)
     extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(descval = Some(d))
    def pub = copy(vis = true)
    def priv = copy(vis = false)
    def file(content: String, name: String = "f%s" format filevals.size) =
      copy(filevals = filevals + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost / "gists" << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val js =
        ("public" -> vis) ~ 
        ("description" -> jStringOrNone(descval)) ~
        ("files" -> filevals.map {
          case (name, content) => (name -> ("content" -> content))
        })
      compact(render(js))
    }
  }

  case class RegistBuilder(id: String,
                           filevals: Map[String, String] = Map.empty[String, String],
                           descval: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(descval = Some(d))
    def file(content: String, name: String = "f%s" format filevals.size) =
      copy(filevals = filevals + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "gists" / id << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val js =
        ("description" -> jStringOrNone(descval)) ~
        ("files" -> filevals.map {
          case (name, content) => (name -> ("content" -> content))
        })
      compact(render(js))
    }
  }

  def gists = new {

    def user(user: String) =
      complete(apiHost / "users" / user / "gists")

    def list(since: Option[String]) =
      complete(apiHost / "gists")

    def get(id: String) =
      complete(apiHost / "gists" / id)

    def post =
      GistBuilder()

    def regist(id: String) =
      RegistBuilder(id)

    def star(id: String) = // 411 length required
      complete(apiHost.PUT / "gists" / id / "star")

    def unstar(id: String) = 
      complete(apiHost.DELETE / "gists" / id / "star")

    def stared(id: String) =
      complete(apiHost.GET / "gists" / id / "star")

    def fork(id: String) = 
      complete(apiHost.POST / "gists" / id / "fork")

    def delete(id: String) = 
      complete(apiHost.DELETE / "gists" / id)
  }
}
