package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder
import java.util.Date

// cli.issues
// cli.userissues
// cli.orgissues

/** http://developer.github.com/v3/issues/#list-issues */
trait Issues { self: Requests =>
  
  def issues = // not found?
    complete(apiHost / "issues")

  def userissues =
    complete(apiHost / "user" / "issues")

  def orgissues(org: String) =
    complete(apiHost / "orgs" / org / "issues")
}

// cli.repo(user, repo).issues.since(time)...
// cli.repo(user, repo).issues.open(title)...
// cli.repo(user, repo).issue(id)
// cli.repo(user, repo).reissue(id).title(...)...
trait RepoIssues { self: RepoRequests  =>

  protected [this]
  case class RepoIssuesBuilder(user: String,
                               repo: String,
                               milestoneval: String = "none",
                               state: String = "open",
                               assigneeval: Option[String] = None,
                               creatorval: Option[String] = None,
                               mentionedval: Option[String] = None,
                               labelsval: Option[Traversable[String]] = None,
                               sort: String = "created",
                               order: String = "desc",
                               sinceval: Option[String] = None,
                               accept: String = Types.GithubJson)
    extends Client.Completion {

    /** http://developer.github.com/v3/issues/#create-an-issue */
    def open(title: String) =
      RepoIssueBuilder(user, repo).title(title)
    
    /** http://developer.github.com/v3/issues/assignees/#list-assignees */
    def assignees =
      complete(apiHost  / "repos" / user / repo / "assignees")

    /** http://developer.github.com/v3/issues/assignees/#check-assignee */
    def assigned(assignee: String) =
      complete(apiHost / "repos" / user / repo / "assignees" / assignee)

    def accepting = new {
      def raw = copy(accept = Types.RawJson)
      def text = copy(accept = Types.TextJson)
      def html = copy(accept = Types.HtmlJson)
      def fullJson = copy(accept = Types.FullJson)
    }

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

    def sortBy = new {
      def created = copy(sort = "created")
      def updated = copy(sort = "updated")
      def comments = copy(sort = "comments")
    }

    // ordering

    def asc = copy(order = "asc")
    def desc = copy(order = "desc")

    def since(d: Date) = copy(sinceval = Some(ISO8601(d)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "issues" <<? pmap <:< Map("Accept" -> accept))(handler)

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

  protected [this]
  case class RepoIssueBuilder(user: String, repo: String,
                              id: Option[Int] = None,
                              titleval: Option[String] = None,
                              bodyval: Option[String] = None,
                              assigneeval: Option[String] = None,
                              milestoneval: Option[Int] = None,
                              labelsval: Option[Seq[String]] = None,
                              state: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def title(t: String) = copy(titleval = Some(t))
    def body(b: String) = copy(bodyval = Some(b))
    def assignee(a: String) = copy(assigneeval = Some(a))
    def milestone(m: Int) = copy(milestoneval = Some(m))
    def labels(ls: Seq[String]) = copy(labelsval = Some(ls))
    def close = copy(state = Some("close"))
    def open = copy(state = Some("open"))

    override def apply[T](handler: Client.Handler[T]) =
      request(id.map(apiHost.PATCH / "repos" / user / repo / "issues" / _.toString).getOrElse(
        apiHost.POST / "repos" / user / repo / "issues"
      ) << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js =
        ("title" -> jStringOrNone(titleval)) ~
        ("body" -> jStringOrNone(bodyval)) ~
        ("assignee" -> jStringOrNone(assigneeval)) ~
        ("milestone" -> jIntOrNone(milestoneval)) ~
        ("labels" -> labelsval.map(_.toList)) ~
        ("state" -> jStringOrNone(state))
      
      compact(render(js))
    }
  }

  def issues = RepoIssuesBuilder(user, repo)

  protected [this] 
  object Milestones {
    /** http://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository */
    def find =
      complete(apiHost / "repo" / user / repo / "milestones")

    /** http://developer.github.com/v3/issues/milestones/#get-a-single-milestone */
    def get(num: String) =
      complete(apiHost / "repo" / user / repo / "milestones" / num)

    /** http://developer.github.com/v3/issues/milestones/#update-a-milestone */
    def edit(num: String) =
      complete(apiHost.PATCH / "repo" / user / repo / "milestones" / num)

    /** http://developer.github.com/v3/issues/milestones/#delete-a-milestone */
    def delete(num: String) =
      complete(apiHost.DELETE / "repo" / user / repo / "milestones" / num)

    /** http://developer.github.com/v3/issues/milestones/#create-a-milestone */
    def create(num: String) =
      complete(apiHost.POST / "repo" / user / repo / "milestones")
  }

  def milestones = Milestones

  protected [this]
  object Labels extends Client.Completion {
    /** http://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository */
    def apply[T](hand: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "labels")(hand)

    /** http://developer.github.com/v3/issues/labels/#get-a-single-label */
    def get(name: String) =
      complete(apiHost / "repos" / user / repo / "labels" / name)

   /** http://developer.github.com/v3/issues/labels/#create-a-label */
   def create(name: String, color: String) = {
     import org.json4s.JsonDSL._
     import org.json4s.native.Printer.compact
     import org.json4s.native.JsonMethods.render
     val js =
       ("name" -> name) ~
       ("color" -> color)
     complete(apiHost.POST / "repos" / user / repo / "labels" << compact(render(js)))
   }

   /** http://developer.github.com/v3/issues/labels/#update-a-label */
   def edit(name: String, color: String) = {
     import org.json4s.JsonDSL._
     import org.json4s.native.Printer.compact
     import org.json4s.native.JsonMethods.render
     val js =
       ("name" -> name) ~
       ("color" -> color)
     complete(apiHost.PATCH / "repos" / user / repo / "labels" << compact(render(js)))
   }

   /** http://developer.github.com/v3/issues/labels/#delete-a-label */
   def delete(name: String) =
     complete(apiHost.DELETE / "repos" / user / repo / "labels" / name)
  }
  
  def labels = Labels

  protected [this]
  case class Issue(id: Int, accept: String = Types.GithubJson)
     extends Client.Completion {

    def accepting = new {
      def raw = copy(accept = Types.RawJson)
      def text = copy(accept = Types.TextJson)
      def html = copy(accept = Types.HtmlJson)
      def fullJson = copy(accept = Types.FullJson)
    }

    def labels =
      complete(apiHost / "repos" / user  / repo / "issues" / id.toString / "labels")

    /** http://developer.github.com/v3/issues/labels/#add-labels-to-an-issue */
    def label(labs: String*) = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js = JArray(labs.toList map(new JString(_)))
      complete(apiHost.POST / "repos" / user / repo / "issues" / id.toString / "labels" << compact(render(js)))
    }

    /** http://developer.github.com/v3/issues/labels/#replace-all-labels-for-an-issue */
    def relabel(labs: String*) = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js = JArray(labs.toList map(new JString(_)))
      complete(apiHost.PATCH / "repos" / user / repo / "issues" / id.toString / "labels" << compact(render(js)))
    }

    /** http://developer.github.com/v3/issues/labels/#remove-all-labels-from-an-issue */
    def delabel = 
      complete(apiHost.DELETE / "repos" / user / repo / "issues" / id.toString / "labels")

    /** http://developer.github.com/v3/issues/labels/#remove-a-label-from-an-issue */
    def delabel(name: String) =
      complete(apiHost.DELETE / "repos" / user / repo / "issues" / id.toString / "labels" / name)

     /** http://developer.github.com/v3/issues/#get-a-single-issue */
    override def apply[T](hand: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "issues" / id.toString <:< Map("Accept" -> accept))(hand)

    def close =
      RepoIssueBuilder(user, repo, id = Some(id)).close

    protected [this] object Comments extends Client.Completion {
      def get(cid: Int) =
        complete(apiHost / "repos" / user / repo / "issues" / id.toString / "comments" / cid.toString)

      def create(body: String) = {
        import org.json4s.JsonDSL._
        import org.json4s.native.Printer.compact
        import org.json4s.native.JsonMethods.render
        complete(apiHost.POST / "repos" / user / repo / "issues" / id.toString / "comments" << compact(render(("body" -> body))))
      }

      def edit(cid: Int, body: String) = {
        import org.json4s.JsonDSL._
        import org.json4s.native.Printer.compact
        import org.json4s.native.JsonMethods.render
        complete(apiHost.PATCH / "repos" / user / repo / "issues" / "comments" / cid.toString << compact(render(("body" -> body))))
      }

      def delete(cid: Int) =
        complete(apiHost.DELETE / "repos" / user / repo / "issues" / "comments" / cid.toString)

      def apply[T](hand: Client.Handler[T]) =
        request(apiHost / "repos" / user / repo / "issues" / id.toString / "comments")(hand)
    }

    def comments = Comments
  }

  def issue(id: Int) =
    Issue(id)

  /** http://developer.github.com/v3/issues/#edit-an-issue */
  def reissue(id: Int) =
    RepoIssueBuilder(user, repo, id = Some(id))  
}
