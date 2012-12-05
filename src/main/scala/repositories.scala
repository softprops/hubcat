package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Repositories { self: Requests =>

  case class RepoBuilder(name: String, org: Option[String] = None,
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
  
  def repos =
    complete(apiHost / "user" / "repos")

  def userRepos(user: String) =
    complete(apiHost / "users" / user / "repos")

  def orgRepos(org: String) =
    complete(apiHost / "orgs" / org / "repos")

  def respositories =
    complete(apiHost / "repositories")

  def newRepo(name: String) =
    RepoBuilder(name)

  def newOrgRepo(org: String, name: String) =
    RepoBuilder(name, org = Some(org))

  def userRepo(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo)

  def userRepoEdit(user: String, repo: String) =
    complete(apiHost.PATCH / "repos" / user / repo)

  def contributors(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo / "contributors")

  def languages(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo / "languages")

  def teams(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo / "teams")

  def tags(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo / "tags")

  def branches(user: String, repo: String) =
    complete(apiHost / "repos" / user / repo / "branches")

  def branch(user: String, repo: String, br: String) =
    complete(apiHost / "repos" / user / repo / "branches" / br)

  def debranch(user: String, repo: String, br: String) =
    complete(apiHost.DELETE / "repos" / user / repo / "branches" / br)
}
