package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

// application/json
//application/vnd.github.VERSION.raw
trait Git { self: Client =>
  def repo(user: String, repo: String) = new {

    // blobs
    def blob(sha: String, encoding: String) =
      complete(apiHost / "repos" / user / repo / "blobs" / sha)

    def newBlob(content: String, encoding: String) =
      complete(apiHost.POST / "repos" / user / repo / "blobs")

    // commits
    def commit(sha: String) =
      complete(apiHost / "repos" / user / repo / "commits" / sha)

    // http://developer.github.com/v3/git/commits/#create-a-commit

    def newCommit(message: String, tree: String, parents: Traversable[String]) =
      complete(apiHost.POST / "repos" / user / repo / "commits")


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
      complete(apiHost / "repos" / user / repo / "tags" / sha)

    def newTag(tag: String, msg: String, obj: String, tpe: String) =
      complete(apiHost.POST / "repos" / user / repo / "tags")

    def tree(sha: String, recursive: Boolean = false) =
      complete(apiHost / "repos" / user / repo / "trees" / sha)

    def newTree(sha: String, basetree: Option[String] = None, tree: Traversable[String]) =
      complete(apiHost.POST / "repos" / user / repo / "trees" / sha)
  } 
}
