package hubcat

import com.ning.http.client.Response
import org.json4s.JsonDSL._

trait RepoPulls
  extends Client.Completion[Response] { self: RepoRequests =>
  private def base =
    apiHost / "repos" / user / repo / "pulls"

  object pulls extends Client.Completion[List[PullReq]] {

    object comments
      extends Client.Completion[Response] {
      private [this] def base =
        apiHost / "repos" / user / repo / "pulls" / "comments"

      case class Filter(
        _sort: Option[String]      = None,
        _direction: Option[String] = None)
        extends Client.Completion[Response] {
        /** http://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request */
        override def apply[T](handler: Client.Handler[T]) =
          request(base <<? Map.empty[String, String] ++
                  _sort.map("sort" -> _) ++
                  _direction.map("direction" -> _))(handler)
      }

      case class Comment(id: Int) extends Client.Completion[Response] {
        /** http://developer.github.com/v3/pulls/comments/#get-a-single-comment */
        override def apply[T](handler: Client.Handler[T]) =
          request(base  / id)(handler)

        /** http://developer.github.com/v3/pulls/comments/#edit-a-comment */
        def edit(body: String) = {
          val payload = json.str(("body" -> body))
          complete[Response](base.POST / id << payload)
        }

        /** http://developer.github.com/v3/pulls/comments/#delete-a-comment */
        def delete = complete[Response](base.DELETE / id)
      }

      override def apply[T](handler: Client.Handler[T]) =
        filter(handler)

      def filter = Filter()

      def get(id: Int) = Comment(id)
    }

    case class Filter(
      _state: Option[String] = None,
      _head: Option[String] = None,
      _base: Option[String] = None,
      _accept: String = Accept.GithubJson)
      extends Client.Completion[Response] {
      def state(s: String) = copy(_state = Some(s))
      def head(h: String) = copy(_head = Some(h))
      def base(b: String) = copy(_head = Some(b))
      def accepting = new {
        def raw = copy(_accept = Accept.RawJson)
        def text = copy(_accept = Accept.TextJson)
        def html = copy(_accept = Accept.HtmlJson)
        def fullJson = copy(_accept = Accept.FullJson)
      }
      override def apply[T](handler: Client.Handler[T]) =
        request(RepoPulls.this.base <:< Map("Accept" -> _accept) <<? Map.empty[String, String] ++
                _state.map("state" -> _) ++
                _head.map("head" -> _)   ++
                _base.map("base" -> _))(handler)
    }

    /** http://developer.github.com/v3/pulls/#list-pull-requests */
    def filter = Filter()

    /** http://developer.github.com/v3/pulls/#list-pull-requests */
    override def apply[T](handler: Client.Handler[T]) =
      filter(handler)

    /** http://developer.github.com/v3/pulls/#create-a-pull-request */
    def create(title: String, head: String) =
      PullBuilder(title, head)
  }

  /** Operations defined for a specific pull request */
  case class Pull(
    id: Int,
    _accept: String = Accept.GithubJson)
    extends Client.Completion[PullReq] {

    private def acceptHeader = Map("Accept" -> _accept)

    object comments extends Client.Completion[Response] {
      private [this] def base = apiHost / "repos" / user / repo / "pulls" / id / "comments"

      /** http://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request */
      override def apply[T](handler: Client.Handler[T]) =
        request(base)(handler)

      /** Starts a new thread of review. http://developer.github.com/v3/pulls/comments/#create-a-comment */
      def create(body: String, commit: String, path: String, position: Int) = {
        val payload = json.str(
          ("body" -> body) ~
          ("commit_id" -> commit) ~
          ("path" -> path) ~
          ("position" -> position))
        complete[Response](base.POST << payload)
      }

      /** Creates a response in reply to a thread of review. http://developer.github.com/v3/pulls/comments/#create-a-comment */
      def reply(to: Int, body: String) = {
        val payload = json.str(
          ("body" -> body) ~ ("in_reply_to" -> to))
        complete[Response](base.POST << payload)
      }
    }

    /** Update operation fields */
    case class Update(
      _title: Option[String] = None,
      _body: Option[String] = None,
      _state: Option[String] = None)
      extends Client.Completion[PullReq] {
      def title(t: String) = copy(_title = Some(t))
      def body(b: String) = copy(_body = Some(b))
      def state(s: String) = copy(_state = Some(s))
      override def apply[T](handler: Client.Handler[T]) =
        request(base.PATCH / id << body)(handler)
     def body = json.str(
       ("title" -> _title) ~
       ("body"  -> _body) ~
       ("state" -> _state))
    }

    def accepting = new {
      def raw = copy(_accept = Accept.RawJson)
      def text = copy(_accept = Accept.TextJson)
      def html = copy(_accept = Accept.HtmlJson)
      def fullJson = copy(_accept = Accept.FullJson)
      def diff = copy(_accept = Accept.Diff)
      def patch = copy(_accept = Accept.Patch)
    }

    /** http://developer.github.com/v3/pulls/#get-a-single-pull-request */
    override def apply[T](handler: Client.Handler[T]) =
      request(base / id <:< acceptHeader)(handler)

    /** http://developer.github.com/v3/pulls/#update-a-pull-request */
    def update = Update()

    /** http://developer.github.com/v3/pulls/#list-commits-on-a-pull-request */
    def commits = complete[Response](base / id / "commits" <:< acceptHeader)

    /** http://developer.github.com/v3/pulls/#list-pull-requests-files */
    def files = complete[Response](base / id / "files" <:< acceptHeader)

    /** http://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged */
    def merged = complete[Unit](base / id / "merge")

    /** http://developer.github.com/v3/pulls/#merge-a-pull-request-merge-buttontrade */
    def merge(msg: Option[String] = None) = {
      val body = json.str(("commit_message" -> msg))
      complete[MergeResult](base.PUT / id / "merge" << body)
    }
  }

  /** Builder for creating a new pull request */
  case class PullBuilder(
    title: String,
    head: String,
    _base: String         = "master",
    _body: Option[String] = None,
    _issue: Option[Int]   = None)
    extends Client.Completion[PullReq] {
    def body(b: String) = copy(_body = Some(b))
    def base(b: String) = copy(_base = b)
    override def apply[T](handler: Client.Handler[T]) =
      request(RepoPulls.this.base.POST
              << body)(handler)
    def body = json.str(
      ("title" -> title) ~
      ("body" -> _body) ~
      ("base" -> _base) ~
      ("head" -> head) ~
      ("issue" -> _issue))
  }

  def pull(id: Int): Pull = Pull(id)
}
