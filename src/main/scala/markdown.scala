package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

object Markdown {
  def Plain = "markdown"
  def Gfm = "gfm"
}

trait Markdown { self: Requests =>
  import Markdown._
  case class MarkdownBuilder(text: String,
                             modeval: Option[String] = None,
                             contextval: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def context(user: String, repo: String) =
      copy(contextval = Some("%s/%s" format(user, repo)))
    def plain =
      copy(modeval = Some(Plain))
    def githubFlavored =
      copy(modeval = Some(Gfm))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "markdown" << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js = ("text" -> text) ~
               ("mode" -> jStringOrNone(modeval)) ~
               ("context" -> jStringOrNone(contextval))

      compact(render(js))
    }
  }

  def markdown(text: String) = MarkdownBuilder(text)
}
