package hubcat

import com.ning.http.client.Response

trait Searching { self: Requests =>
  object Search {
    def issues(user: String, repo: String, open: Boolean, term: String) =
      complete[Response](apiHost / "legacy" /  "issues" / user / repo / (if(open) "open" else "closed") / term)

    def repos(term: String) =
      complete[Response](apiHost / "legacy" / "repos" / "search" / term)

    def user(term: String) =
      complete[Response](apiHost / "legacy" / "user" / "search" / term)
  }

  def search = Search
}
