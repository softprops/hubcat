package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

//application/vnd.github.VERSION.raw+json
//application/vnd.github.VERSION.text+json
// application/vnd.github.VERSION.html+json
// application/vnd.github.VERSION.full+json
trait Issues { self: Requests =>
  
  def issues = // not found?
    complete(apiHost / "issues")

  def userissues =
    complete(apiHost / "user" / "issues")

  def orgissues(org: String) =
    complete(apiHost / "orgs" / org / "issues")

  case class RepoIssuesBuilder(user: String,
                               repo: String,
                               milestoneval: String = "*",
                               state: String = "open",
                               assigneeval: Option[String] = None,
                               creatorval: Option[String] = None,
                               mentionedval: Option[String] = None,
                               labelsval: Option[Traversable[String]] = None,
                               sort: String = "created",
                               order: String = "desc",
                               sinceval: Option[String] = None)
    extends Client.Completion {

    // states (defaults to open)

    def open = copy(state = "open")
    def closed = copy(state = "closed")

    // milestones

    def milestone(n: Int) = copy(milestoneval = n.toString)
    def noMilestone = copy(milestoneval = "none")
    def anyMilestone = copy(milestoneval = "*")

    // assignees

    def assignee(login: String) = copy(assigneeval = Some(login))
    def unassigned = copy(assigneeval = Some("none"))
    def anyAssignee = copy(assigneeval = Some("*"))

    def creator(login: String) = copy(creatorval = Some(login))
    
    def mentioned(login: String) = copy(mentionedval = Some(login))
    
    def labels(l: Traversable[String]) =
      copy(labelsval = Some(l))

    // sorting

    def sortByCreated = copy(sort = "created")
    def sortByUpdated = copy(sort = "updated")
    def sortyByComments = copy(sort = "comments")

    // ordering

    def asc = copy(order = "asc")
    def desc = copy(order = "desc")

    def since(l: Long) = this // todo format date

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "issues" <<? pmap)(handler)

    private def pmap =
      Map("milestone" -> milestoneval.toString,
          "state"     -> state,
          "sort"      -> sort,
          "order"     -> order) ++
      assigneeval.map("assignee" -> _) ++
      creatorval.map("creator" -> _) ++
      mentionedval.map("mentioned" -> _) ++
      labelsval.map("labels" -> _.mkString(","))
  }

  case class RepoIssueBuilder(user: String, repo: String,
                              id: Option[Int] = None,
                              titleval: Option[String] = None,
                              bodyval: Option[String] = None,
                              assigneeval: Option[String] = None,
                              milestoneval: Option[Int] = None,
                              labelsval: Option[Seq[String]] = None)
     extends Client.Completion
        with Jsonizing {

    def title(t: String) = copy(titleval = Some(t))
    def body(b: String) = copy(bodyval = Some(b))
    def assignee(a: String) = copy(assigneeval = Some(a))
    def milestone(m: Int) = copy(milestoneval = Some(m))
    def labels(ls: Seq[String]) = copy(labelsval = Some(ls))

    override def apply[T](handler: Client.Handler[T]) =
      request(id.map(apiHost.PUT / "repos" / user / repo / "issues" / _.toString).getOrElse(
        apiHost.POST / "repos" / user / repo
      ) << pjson)(handler)

    private def pjson = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val js =
        ("title" -> jStringOrNone(titleval)) ~
        ("body" -> jStringOrNone(bodyval)) ~
        ("assignee" -> jStringOrNone(assigneeval)) ~
        ("milestone" -> jIntOrNone(milestoneval))
        ("labels" -> labelsval.map(_.toList))
      
      compact(render(js))
    }
  }

  def repoissues(user: String, repo: String) =
    RepoIssuesBuilder(user, repo)

  def repoissue(user: String, repo: String, id: String) =
    complete(apiHost / "repos" / user / repo / "issues" / id)

   def newRepoIssue(user: String, repo: String, title: String) =
     RepoIssueBuilder(user, repo).title(title)

   def reissue(user: String, repo: String, id: Int) =
     RepoIssueBuilder(user, repo, id = Some(id))
}
