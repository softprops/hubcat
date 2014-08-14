package hubcat

import com.ning.http.client.Response

// application/json
//application/vnd.github.VERSION.raw
trait Git { self: RepoRequests =>
  case class TreeQueryBuilder(sha: String, recur: Option[Int] = None)
     extends Client.Completion[Response] {

    def recursive = copy(recur = Some(1))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost / "repos" / user / repo / "git" / "trees" / sha <<? pmap)(handler)
    private def pmap = Map.empty[String, String] ++ recur.map("recusive" -> _.toString)
  }

  /** http://developer.github.com/v3/git/blobs/ */
  case class BlobQueryBuilder(sha: String, rawval: Boolean = false)
     extends Client.Completion[Response] {
   
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
