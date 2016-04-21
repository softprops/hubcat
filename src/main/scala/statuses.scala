package hubcat

import dispatch._
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

object Status {
  sealed trait State {
    def value: String
  }
  object Pending extends State {
    val value = "pending"
  }
  object Success extends State {
    val value = "success"
  }
  object Error extends State {
    val value = "error"
  }
  object Failure extends State {
    val value = "failure"
  }
}

trait RepoStatuses { self: RepoRequests =>
  case class Statuses(ref: String)
    extends Client.Completion {
    private [this] def base = apiHost / "repos" / user / repo / "statuses" / ref

    case class StatusBuilder(
      state: Status.State,
      _targetUrl: Option[String] = None,
      _desc: Option[String] = None,
      _context: Option[String] = None)
      extends Client.Completion {
      def targetUrl(target: String) = copy(_targetUrl = Some(target))
      def desc(d: String) = copy(_desc = Some(d))
      def context(c: String) = copy(_context = Some(c))
      override def apply[T](handler: Client.Handler[T]) =
        request(base.POST << pmap)(handler)

      private def pmap =
        compact(render(("state" -> state.value) ~
                       ("target_url" -> _targetUrl) ~
                       ("description" -> _desc) ~
                       ("context" -> _context)))
    }

    /** http://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref */
    override def apply[T](handler: Client.Handler[T]) =
      request(base)(handler)

    /** http://developer.github.com/v3/repos/statuses/#create-a-status */
    def create(state: Status.State) =
      StatusBuilder(state)
  }

  /** http://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref */
  def statuses(ref: String) = Statuses(ref)
}
