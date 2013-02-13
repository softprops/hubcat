package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

import org.json4s.JsonDSL._
import org.json4s.native.Printer.compact
import org.json4s.native.JsonMethods.render

// cli.repositories.all
// cli.repositories.owned
// cli.repositories.create(name).desc(...)...
// cli.user(foo).repos....
// cli.repo(user, repo)...
trait Repositories { self: Requests =>
  protected [this] case class RepoBuilder(name: String,
                         org: Option[String] = None,
                         descval: Option[String] = None,
                         teamId: Option[Int] = None,
                         homepageval: Option[String] = None,
                         hasIssues: Boolean = true,
                         hasWiki: Boolean = true,
                         hasDownloads: Boolean = true,
                         autoinit: Boolean = false,
                         ignoreTemplate: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(descval = Some(d))
    def homepage(h: String) = copy(homepageval = Some(h))
    def issues(b: Boolean) = copy(hasIssues = b)
    def wiki(b: Boolean) = copy(hasWiki = b)
    def downloads(b: Boolean) = copy(hasDownloads = b)
    def team(id: Int) = copy(teamId = Some(id))
    def autoInit(a: Boolean) = copy(autoinit = a)
    def gitignoreTemplate(t: String) = copy(ignoreTemplate = Some(t))
    def underOrganization(o: String) = copy(org = Some(o))
    override def apply[T](handler: Client.Handler[T]) =
      request(
        org.map(o => apiHost.POST / "orgs" / o / "repos")
           .getOrElse(apiHost.POST / "user" / "repos") << pjson)(handler)

    private def pjson = {
      val js =
        ("name" -> name) ~
        ("description" -> jStringOrNone(descval)) ~
        ("homepage" -> jStringOrNone(homepageval)) ~
        ("has_issues" -> hasIssues) ~
        ("has_wiki" -> hasWiki) ~
        ("has_downloads" -> hasDownloads) ~
        ("team_id" -> jIntOrNone(teamId)) ~
        ("auto_init" -> autoinit) ~
        ("gitignore_template" -> jStringOrNone(ignoreTemplate))
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
  case class RepoFilter(base: RequestBuilder,
                         typ: String = "all",
                         sort: String = "full_name",
                         dir: Option[String] = None)
     extends Client.Completion {

    // type

    def all = copy(typ = "all")
    def owner = copy(typ = "owner")
    def pub = copy(typ = "public")
    def priv = copy(typ = "private")
    def member = copy(typ = "member")
 
    // sort

    def sortBy = new {
      def created = copy(sort = "created")
      def updated = copy(sort = "updated")
      def pushed = copy(sort = "pushed")
      def fullName = copy(sort = "full_name")
    }

    // dir

    def asc = copy(dir = Some("asc"))
    def desc = copy(dir = Some("desc"))

    override def apply[T](handler: Client.Handler[T]) =
      request(base <<? Map("type" -> typ,
                           "sort" -> sort) ++
                         dir.map("direction" -> _))(handler)
  }

  def repositories = new {
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

  def organization(org: String) = new {
    /** http://developer.github.com/v3/repos/#list-organization-repositories */
    def repos =
      complete(apiHost / "orgs" / org / "repos")
  }

  def repo(login: String, name: String) =
    user(login).repo(name)

  def user(user: String) =
    new UserRequests(user)
}

class RepoRequests(val user: String, val repo: String, requests: Requests)
    extends Client.Completion
       with Git
       with RepoIssues {

    // for mixins
    def request[T](req: RequestBuilder)(handler: Client.Handler[T]): Promise[T] =
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

    protected [this]
    object Hooks extends Client.Completion with Jsonizing {
      private [this] def base = apiHost / "repos" / user / repo / "hooks"

      protected [this]
      case class Hook(name: String,
                      id: Option[String] = None,
                      configval: Map[String, Any] = Map.empty[String, Any],
                      eventsval: List[String] = List("push"),
                      activeval: Option[Boolean] = None,
                      addeventsval: List[String] = Nil,
                      rmeventsval: List[String] = Nil)
         extends Client.Completion {

         def config(props: (String, Any)*) =
           copy(configval = props.toMap)

         def events(es: String*) =
           copy(eventsval = es.toList)

         def addEvents(es: String*) =
           id match {
             case Some(_) => copy(addeventsval = es.toList)
             case _ => copy(eventsval = eventsval ::: es.toList)
           }

         def removeEvents(es: String*) =
           id match {
             case Some(_) => copy(rmeventsval = es.toList)
             case _ => this
           }

         def active(is: Boolean) =
           copy(activeval = Some(is))

         override def apply[T](hand: Client.Handler[T]) =
           request(id.map(base.PATCH / _).getOrElse(base.POST) << pmap)(hand)

         private def pmap = {
           val json = ("name" -> name) ~ ("events" -> eventsval) ~ ("active" -> jBoolOrNone(activeval))
           val confd = if (configval.isEmpty) json else json ~ ("config" -> configval.map {
             case (k, v) => (k -> v.toString)
           })
           compact(render(if (id.isDefined) confd ~ ("add_events" -> addeventsval) ~ ("remove_events" -> rmeventsval)
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
                      secretval: Option[String] = None)
        extends Client.Completion {
      private [this] def base = apiHost / "hub"
      def secret(sec: String) = copy(secretval = Some(sec))

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
        secretval.map("hub.secret" -> _)
    }

    def subscribe(event: String, callback: String) =
      PubHub("subscribe", event, callback)

    def unsubscribe(event: String, callback: String) =
      PubHub("unsubscribe", event, callback)
}
