package hubcat

import dispatch._

trait Searching { self: Requests =>
  object Search {
    def issues(user: String, repo: String, open: Boolean, term: String) =
      complete(apiHost / "legacy" /  "issues" / user / repo / (if(open) "open" else "closed") / term)

    def repos(term: String) =
      complete(apiHost / "legacy" / "repos" / "search" / term)

    def user(term: String) =
      complete(apiHost / "legacy" / "user" / "search" / term)
  }

  def search = Search
}
