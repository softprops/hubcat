package hubcat

import dispatch._

object Markdown {
  def Plain = "markdown"
  def Gfm = "gfm"
}

trait Markdown { self: Requests =>
  import Markdown._
  case class MarkdownBuilder(
    text: String,
    _mode: Option[String] = None,
    _context: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def context(user: String, repo: String) =
      copy(_context = Some("%s/%s" format(user, repo)))
    def plain =
      copy(_mode = Some(Plain))
    def githubFlavored =
      copy(_mode = Some(Gfm))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "markdown" << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js = ("text" -> text) ~
               ("mode" -> jStringOrNone(_mode)) ~
               ("context" -> jStringOrNone(_context))

      compact(render(js))
    }
  }

  def markdown(text: String) = MarkdownBuilder(text)
}
