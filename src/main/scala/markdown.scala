package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

// todo: support raw
trait Markdown { self: Client =>
  case class MarkdownBuilder(text: String,
                             modeval: Option[String] = None,
                             contextval: Option[String] = None)
     extends Client.Completion {
    def context(user: String, repo: String) =
      copy(contextval = Some("%s/%s" format(user, repo)))
    def plain =
      copy(modeval = Some("markdown"))
    def githubFlavored =
      copy(modeval = Some("gfm"))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "markdown" << pmap)(handler)

    private def pmap =
      Map("text" -> text) ++
       modeval.map("mode" -> _) ++
       contextval.map("context" -> _)
  }

  def markdown(text: String) = MarkdownBuilder(text)
}
