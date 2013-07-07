package hubcat

import dispatch._
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

trait RepoHooks { self: RepoRequests =>
  class Hooks extends Client.Completion with Jsonizing {
    private [this] def base = apiHost / "repos" / user / repo / "hooks"

    protected [this]
    case class Hook(
      name: String,
      _id: Option[String] = None,
      _config: Map[String, Any] = Map.empty[String, Any],
      _events: List[String] = List("push"),
      _active: Option[Boolean] = None,
      _addevents: List[String] = Nil,
      _rmevents: List[String] = Nil)
      extends Client.Completion {

      def config(props: (String, Any)*) =
        copy(_config = props.toMap)

      def events(es: String*) =
        copy(_events = es.toList)

      def addEvents(es: String*) =
        _id match {
          case Some(_) => copy(_addevents = es.toList)
          case _ => copy(_events = _events ::: es.toList)
        }

      def removeEvents(es: String*) =
        _id match {
          case Some(_) => copy(_rmevents = es.toList)
          case _ => this
        }

      def active(is: Boolean) =
        copy(_active = Some(is))

      override def apply[T](hand: Client.Handler[T]) =
        request(_id.map(base.PATCH / _).getOrElse(base.POST) << pmap)(hand)

      private def pmap = {
        val json = ("name" -> name) ~ ("events" -> _events) ~ ("active" -> jBoolOrNone(_active))
        val confd = if (_config.isEmpty) json else json ~ ("config" -> _config.map {
          case (k, v) => (k -> v.toString)
        })
        compact(render(if (_id.isDefined) confd ~ ("add_events" -> _addevents) ~ ("remove_events" -> _rmevents)
                       else confd))
      }
    }

    /** http://developer.github.com/v3/repos/hooks/#list */
    override def apply[T](hand: Client.Handler[T]) =
      request(base)(hand)

    /** http://developer.github.com/v3/repos/hooks/#get-single-hook */
    def apply(id: String) =
      complete(base / id)

    /** http://developer.github.com/v3/repos/hooks/#create-a-hook */
    def create(name: String) =
      Hook(name)

    /** http://developer.github.com/v3/repos/hooks/#edit-a-hook */
    def edit(id: String, name: String) =
      Hook(name, Some(id))

    /** http://developer.github.com/v3/repos/hooks/#test-a-hook */
    def test(id: String) =
      complete(base.POST / id / "tests")

    /** http://developer.github.com/v3/repos/hooks/#delete-a-hook */
    def delete(id: String) =
      complete(base.DELETE / id)
  }

  def hooks = new Hooks
}
