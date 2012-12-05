package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Markdown { self: Requests =>
  case class MarkdownBuilder(text: String,
                             modeval: Option[String] = None,
                             contextval: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def context(user: String, repo: String) =
      copy(contextval = Some("%s/%s" format(user, repo)))
    def plain =
      copy(modeval = Some("markdown"))
    def githubFlavored =
      copy(modeval = Some("gfm"))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "markdown" << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val js = ("text" -> text) ~
               ("mode" -> jStringOrNone(modeval)) ~
               ("context" -> jStringOrNone(contextval))

      compact(render(js))
    }
  }

  def markdown(text: String) = MarkdownBuilder(text)
}
