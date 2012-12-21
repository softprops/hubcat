package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

// cli.repositories.all
// cli.repositories.owned
// cli.repositories.create(name).desc(...)...
// cli.user(foo).repos....
// cli.repo(user, repo)...
trait Repositories { self: Requests =>
  protected [this]
  case class RepoBuilder(name: String,
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
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
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
  }
