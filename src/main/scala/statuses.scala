package hubcat

import com.ning.http.client.Response
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

object Status {
  sealed trait State {
    def value: String
  }
  abstract class Value(val value: String) extends State
  object Pending extends Value("pending")
  object Success extends Value("success")
  object Error extends Value("error")
  object Failure extends Value("failure")
}

trait RepoStatuses { self: RepoRequests =>
  case class Statuses(ref: String)
    extends Client.Completion[Response] {
    private [this] def base = apiHost / "repos" / user / repo / "statuses" / ref

    case class StatusBuilder(
      state: Status.State,
      _targetUrl: Option[String] = None,
      _desc: Option[String] = None)
      extends Client.Completion[Response] {
      def targetUrl(target: String) = copy(_targetUrl = Some(target))
      def desc(d: String) = copy(_desc = Some(d))
      override def apply[T](handler: Client.Handler[T]) =
        request(base.POST << pmap)(handler)

      private def pmap =
        compact(render(("state" -> state.value) ~
                       ("target_url" -> _targetUrl) ~
                       ("description" -> _desc)))
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
