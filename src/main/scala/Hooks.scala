package hubcat

import org.json4s.JNothing
import org.json4s.JsonDSL._

trait RepoHooks { self: RepoRequests =>
  object Hooks extends Client.Completion[List[hubcat.Hook]] {
    private [this] def base =
      apiHost / "repos" / user / repo / "hooks"

    protected [this]
    case class Hook(
      name: String,
      _id: Option[String]       = None,
      _config: Map[String, Any] = Map.empty[String, Any],
      _events: List[String]     = List("push"),
      _active: Option[Boolean]  = None,
      _addevents: List[String]  = Nil,
      _rmevents: List[String]   = Nil)
      extends Client.Completion[hubcat.Hook] {

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
        request(_id.map(base.PATCH / _).getOrElse(base.POST)
                << body)(hand)

        /*(_config match {
          case cfg if cfg.nonEmpty =>
            ("config" -> cfg.map {
              case (k, v) => (k, v.toString)
            }.toMap)
          case _ => ("x" -> JNothing)
        }) ~*/

    def body = json.str(
        ("name"   -> name) ~
        ("events" -> _events) ~
        ("active" -> _active)) /*~
        (_id match {
          case Some(_) =>
            ("add_events"    -> _addevents) ~
            ("remove_events" -> _rmevents)
          case _ =>
            ("x" -> JNothing)
        }))*/
    }

    /** http://developer.github.com/v3/repos/hooks/#list */
    override def apply[T](hand: Client.Handler[T]) =
      request(base)(hand)

    /** http://developer.github.com/v3/repos/hooks/#get-single-hook */
    def apply(id: String) =
      complete[hubcat.Hook](base / id)

    /** http://developer.github.com/v3/repos/hooks/#create-a-hook */
    def create(name: String) =
      Hook(name)

    /** http://developer.github.com/v3/repos/hooks/#edit-a-hook */
    def edit(id: String, name: String) =
      Hook(name, Some(id))

    /** http://developer.github.com/v3/repos/hooks/#test-a-hook */
    def test(id: String) =
      complete[Unit](base.POST / id / "tests")

    /** http://developer.github.com/v3/repos/hooks/#delete-a-hook */
    def delete(id: String) =
      complete[Unit](base.DELETE / id)
  }
}
