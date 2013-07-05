package hubcat

import dispatch._
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

trait RepoPulls
  extends Client.Completion with Jsonizing { self: RepoRequests =>
  private def base = apiHost / "repos" / user / repo / "pulls"
  class Pulls
    extends Client.Completion {
    case class Filter(
      _state: Option[String] = None,
      _head: Option[String] = None,
      _base: Option[String] = None)
      extends Client.Completion {
      def state(s: String) = copy(_state = Some(s))
      def head(h: String) = copy(_head = Some(h))
      def base(b: String) = copy(_head = Some(b))
      override def apply[T](handler: Client.Handler[T]) =        
        request(RepoPulls.this.base <<? Map.empty[String, String] ++
                _state.map("state" -> _) ++
                _head.map("head" -> _)   ++
                _base.map("base" -> _))(handler)
      }
      override def apply[T](handler: Client.Handler[T]) =
        request(RepoPulls.this.base)(handler)

      /** http://developer.github.com/v3/pulls/#create-a-pull-request */
      def create(title: String) =
        PullBuilder(title)
    }

  case class Pull(id: Int) extends Client.Completion {
    case class Update(
      _title: Option[String] = None,
      _body: Option[String] = None,
      _state: Option[String] = None)
      extends Client.Completion {
      def title(t: String) = copy(_title = Some(t))
      def body(b: String) = copy(_body = Some(b))
      def state(s: String) = copy(_state = Some(s))
      override def apply[T](handler: Client.Handler[T]) =
        request(base.PATCH / id << pmap)(handler)
      private def pmap =
        compact(render(("title" -> _title) ~ ("body" -> _body) ~
                       ("state" -> _state)))
      }

      override def apply[T](handler: Client.Handler[T]) =        
        request(base / id)(handler)

      /** http://developer.github.com/v3/pulls/#update-a-pull-request */
      def update = Update()

      /** http://developer.github.com/v3/pulls/#list-commits-on-a-pull-request */
      def commits = complete(base / id / "commits")

      /** http://developer.github.com/v3/pulls/#list-pull-requests-files */
      def files = complete(base / id / "files")

      /** http://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged */
      def merged = complete(base / id / "merged")

      /** http://developer.github.com/v3/pulls/#merge-a-pull-request-merge-buttontrade */
      def merge = complete(base.PUT / id / "merged")
  }

  case class PullBuilder(
    title: String,
    _body: Option[String] = None,
    _base: Option[String] = None,
    _head: Option[String] = None,
    _issue: Option[Int] = None)
    extends Client.Completion {
    def body(b: String) = copy(_body = Some(b))
    def base(b: String) = copy(_base = Some(b))
    def head(h: String) = copy(_head = Some(h))
    override def apply[T](handler: Client.Handler[T]) =        
      request(RepoPulls.this.base.POST << pmap)(handler)
    def pmap =
      compact(render(("title" -> title) ~ ("body" -> _body) ~
          ("base" -> _base) ~ ("head" -> _head) ~ ("issue" -> _issue)))
    }

    /** http://developer.github.com/v3/pulls/#list-pull-requests */
    def pulls = new Pulls

    /** http://developer.github.com/v3/pulls/#get-a-single-pull-request */
    def pull(id: Int): Pull = Pull(id)
}
