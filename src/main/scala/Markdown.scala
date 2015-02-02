package hubcat

import org.json4s.JsonDSL._

trait Markdown { self: Requests =>
  case class MarkdownBuilder(
    text: String,
    _mode: Option[String]    = None,
    _context: Option[String] = None)
     extends Client.Completion[String] {

    def context(user: String, repo: String) =
      copy(_context = Some(s"$user/$repo"))
    def plain =
      copy(_mode = Some("markdown"))
    def githubFlavored =
      copy(_mode = Some("gfm"))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "markdown"
              << body)(handler)

    def body = json.str(
      ("text" -> text) ~
      ("mode" -> _mode) ~
      ("context" -> _context))
  }

  def markdown(text: String) = MarkdownBuilder(text)
}
