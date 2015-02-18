package hubcat

import dispatch._
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

// application/json
//application/vnd.github.VERSION.raw
trait Git { self: RepoRequests =>
  case class TreeQueryBuilder(sha: String, recur: Option[Int] = None)
     extends Client.Completion {

    def recursive = copy(recur = Some(1))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "git" / "trees" / sha <<? pmap)(handler)
    private def pmap = Map.empty[String, String] ++ recur.map("recusive" -> _.toString)
  }

  /** http://developer.github.com/v3/git/blobs/ */
  case class BlobQueryBuilder(sha: String, rawval: Boolean = false)
     extends Client.Completion {
   
    def raw = copy(rawval = true)

    override def apply[T](handler: Client.Handler[T]) = {
      val req = apiHost / "repos" / user / repo / "git" / "blobs" / sha
      request(if (rawval) req <:< Map("Accept" -> Accept.Raw) else req)(handler)
    }
  }

  // blobs
    
  def blob(sha: String) =
    BlobQueryBuilder(sha)

  def newBlob(content: String, encoding: String = "utf-8") =
    complete(apiHost.POST / "repos" / user / repo / "git" / "blobs" << {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      compact(render(("content" -> content) ~ ("encoding" -> encoding)))
    })

  // commits
  def commit(sha: String) =
    complete(apiHost / "repos" / user / repo / "git" / "commits" / sha)

  protected[this]
  case class CommitsFilter(base: Req,
                           _sha: Option[String] = None,
                           _path: Option[String] = None,
                           _author: Option[String] = None,
                           _since: Option[DateTime] = None,
                           _until: Option[DateTime] = None) extends Client.Completion {
    val dateTimeFormat = DateTimeFormat.forPattern("YYYY-MM-DDTHH:MM:SSZ")

    def sha(s: String): CommitsFilter = copy(_sha = Some(s))
    def path(p: String): CommitsFilter = copy(_path = Some(p))
    def author(a: String): CommitsFilter = copy(_author = Some(a))
    def since(s: DateTime): CommitsFilter = copy(_since = Some(s))
    def until(u: DateTime): CommitsFilter = copy(_until = Some(u))

    override def apply[T](handler: Client.Handler[T]): Future[T] = {
      val params = Map() ++
        _sha.map("sha" -> _) ++
        _path.map("path" -> _) ++
        _author.map("author" -> _) ++
        _since.map(s => "since" -> dateTimeFormat.print(s)) ++
        _until.map(u => "until" -> dateTimeFormat.print(u))
      request(base <<? params)(handler)
    }
  }

  /** https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository */
  def commits =
    CommitsFilter(apiHost / "repos" / user / repo / "git" / "commits")

  // http://developer.github.com/v3/git/commits/#create-a-commit

  def newCommit(message: String, tree: String, parents: Traversable[String]) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "commits")


  // refs

  def ref(id: String) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "refs" / id)

  def refs(namespace: Option[String] = None) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "refs")
    
  def newRef(ref: String, sha: String) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "refs")

  def reref(id: String, sha: String, force: Boolean = false) =
    complete(apiHost.PATCH / "repos" / user / repo / "git" / "refs" / id)

  def deref(id: String) =
    complete(apiHost.DELETE / "repos" / user / repo / "git" / "refs" / id)

  // tags

  def tag(sha: String) =
    complete(apiHost / "repos" / user / repo / "git" / "tags" / sha)

  def newTag(tag: String, msg: String, obj: String, tpe: String) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "tags")

  def tree(sha: String, recursive: Boolean = false) =
    TreeQueryBuilder(sha)

  def newTree(sha: String, basetree: Option[String] = None, tree: Traversable[String]) =
    complete(apiHost.POST / "repos" / user / repo / "git" / "trees" / sha)
}
