package hubcat

import com.ning.http.client.RequestBuilder
import dispatch._
import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render
import scala.concurrent.Future

// cli.repositories.all
// cli.repositories.owned
// cli.repositories.create(name).desc(...)...
// cli.user(foo).repos....
// cli.repo(user, repo)...
trait Repositories { self: Requests =>
  /** Builder for creating repositories */
  protected [this] case class RepoBuilder(
    name: String,
    _org: Option[String] = None,
    _desc: Option[String] = None,
    _teamId: Option[Int] = None,
    _homepage: Option[String] = None,
    _hasIssues: Boolean = true,
    _hasWiki: Boolean = true,
    _hasDownloads: Boolean = true,
    _autoinit: Boolean = false,
    _ignoreTemplate: Option[String] = None)
    extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(_desc = Some(d))
    def homepage(h: String) = copy(_homepage = Some(h))
    def issues(b: Boolean) = copy(_hasIssues = b)
    def wiki(b: Boolean) = copy(_hasWiki = b)
    def downloads(b: Boolean) = copy(_hasDownloads = b)
    def team(id: Int) = copy(_teamId = Some(id))
    def autoInit(a: Boolean) = copy(_autoinit = a)
    def gitignoreTemplate(t: String) = copy(_ignoreTemplate = Some(t))
    def underOrganization(o: String) = copy(_org = Some(o))
    override def apply[T](handler: Client.Handler[T]) =
      request(
        _org.map(o => apiHost.POST / "orgs" / o / "repos")
            .getOrElse(apiHost.POST / "user" / "repos") << pjson)(handler)

    private def pjson = {
      val js =
        ("name" -> name) ~
        ("description" -> jStringOrNone(_desc)) ~
        ("homepage" -> jStringOrNone(_homepage)) ~
        ("has_issues" -> _hasIssues) ~
        ("has_wiki" -> _hasWiki) ~
        ("has_downloads" -> _hasDownloads) ~
        ("team_id" -> jIntOrNone(_teamId)) ~
        ("auto_init" -> _autoinit) ~
        ("gitignore_template" -> jStringOrNone(_ignoreTemplate))
      compact(render(js))
    }
  }

  protected [this]
  class UserRequests(user: String) {
    /** http://developer.github.com/v3/repos/#list-user-repositories */
    def repos =
      RepoFilter(apiHost / "users" / user / "repos")

    /** http://developer.github.com/v3/repos/#get */
    def repo(name: String) =
      new RepoRequests(user, name, self)
  }
  
  protected [this]
  case class RepoFilter(
    base: RequestBuilder,
    _typ: String = "all",
    _sort: String = "full_name",
    _dir: Option[String] = None)
     extends Client.Completion {

    // type

    def all = copy(_typ = "all")
    def owner = copy(_typ = "owner")
    def pub = copy(_typ = "public")
    def priv = copy(_typ = "private")
    def member = copy(_typ = "member")
 
    // sort

    def sortBy = new {
      def created = copy(_sort = "created")
      def updated = copy(_sort = "updated")
      def pushed = copy(_sort = "pushed")
      def fullName = copy(_sort = "full_name")
    }

    // dir

    def asc = copy(_dir = Some("asc"))
    def desc = copy(_dir = Some("desc"))

    override def apply[T](handler: Client.Handler[T]) =
      request(base <<? Map("type" -> _typ,
                           "sort" -> _sort) ++
                           _dir.map("direction" -> _))(handler)
  }

  object AnyRepoRequests {
    /** http://developer.github.com/v3/repos/#list-all-repositories */
    case class RepoLimiter(base: RequestBuilder, sinceval: Option[Int] = None)
       extends Client.Completion {
      def since(id: Int) = copy(sinceval = Some(id)) 
      override def apply[T](handler: Client.Handler[T]) =
        request(base <<? Map.empty[String, String] ++
                           sinceval.map("since" -> _.toString))(handler)
    }
    def all =
      RepoLimiter(apiHost / "repositories")

    /** http://developer.github.com/v3/repos/#list-your-repositories */
    def owned =
      RepoFilter(apiHost / "user" / "repos")

    /** http://developer.github.com/v3/repos/#create */
    def create(name: String) =
      RepoBuilder(name)
  }

  def repositories =
    AnyRepoRequests

  case class OrganizationRepoRequests(org: String) {
     /** http://developer.github.com/v3/repos/#list-organization-repositories */
    def repos =
      complete(apiHost / "orgs" / org / "repos")
  }

  def organization(org: String) =
    OrganizationRepoRequests(org)

  def repo(login: String, name: String) =
    user(login).repo(name)

  def user(user: String) =
    new UserRequests(user)
}

/** Repository requests for a specific repo */
class RepoRequests(val user: String, val repo: String, requests: Requests)
    extends Client.Completion
       with Git
       with RepoIssues {

    // for mixins
    def request[T](req: RequestBuilder)(handler: Client.Handler[T]): Future[T] =
      requests.request(req)(handler)
    def complete(req: RequestBuilder): Client.Completion =
      requests.complete(req)
    def apiHost =
      requests.apiHost

     // for "completeness"
    override def apply[T](handler: Client.Handler[T]) =        
      request(apiHost / "repos" / user / repo)(handler)

    /** http://developer.github.com/v3/repos/#edit */
    def edit =
      complete(apiHost.PATCH / "repos" / user / repo)

    /** http://developer.github.com/v3/repos/#delete-a-repository */
    def delete =
      complete(apiHost.DELETE / "repos" / user / repo)

    /** http://developer.github.com/v3/repos/#list-contributors */
    def contributors =
      complete(apiHost / "repos" / user / repo / "contributors")

    /** http://developer.github.com/v3/repos/#list-languages */
    def languages =
      complete(apiHost / "repos" / user / repo / "languages")

    /** http://developer.github.com/v3/repos/#list-languages */
    def teams =
      complete(apiHost / "repos" / user / repo / "teams")

    /** http://developer.github.com/v3/repos/#list-tags */
    def tags =
      complete(apiHost / "repos" / user / repo / "tags")

    /** http://developer.github.com/v3/repos/#list-branches */
    def branches =
      complete(apiHost / "repos" / user / repo / "branches")

    /** http://developer.github.com/v3/repos/#get-branches */
    def branch(br: String) =
      complete(apiHost / "repos" / user / repo / "branches" / br)

    def debranch(br: String) =
      complete(apiHost.DELETE / "repos" / user / repo / "branches" / br)

    /** Gihub hook interfaces */
    protected [this]
    object Hooks extends Client.Completion with Jsonizing {
      private [this] def base = apiHost / "repos" / user / repo / "hooks"

      protected [this]
      case class Hook(name: String,
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

    def hooks = Hooks

    /** http://developer.github.com/v3/repos/hooks/#pubsubhubbub */
    protected [this]
    case class PubHub(mode: String,
                      event: String,
                      callback: String,
                      _secret: Option[String] = None)
        extends Client.Completion {
      private [this] def base = apiHost / "hub"
      def secret(sec: String) = copy(_secret = Some(sec))

      // fixme: will get Needs hub.callback if params
      // provided in request body. escaping issue?
      override def apply[T](hand: Client.Handler[T]) =
        request(base.POST <<? pmap)(hand)

      private def pmap = Map(
        "hub.mode" -> mode,
        "hub.callback" -> callback,
        "hub.topic" -> "https://github.com/%s/%s/events/%s"
                        .format(user, repo, event)
        ) ++
        _secret.map("hub.secret" -> _)
    }

    def subscribe(event: String, callback: String) =
      PubHub("subscribe", event, callback)

    def unsubscribe(event: String, callback: String) =
      PubHub("unsubscribe", event, callback)
}
