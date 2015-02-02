package hubcat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._
import org.json4s.JsonDSL._

// default representations of github resources

case class App(url: String, name: String, clientId: String)

case class Authorization(
  id: Int,
  url: String,
  scopes: List[String],
  token: String,
  app: App,
  note: String,
  noteUrl: String,
  createdAt: String,
  updatedAt: String
)

case class User(
  login: String,
  id: Int,
  typ: String,
  siteAdmin: Boolean,
  links: Map[String, String]
)

case class Repo(
  id: Int,
  owner: User,
  name: String,
  fullName: String,
  description: String,
  privateRepo: Boolean,
  fork: Boolean,
  url: String,
  homepage: String,
  forks: Int,
  starGazers: Int,
  watchers: Int,
  size: Int,
  defaultBranch: String,
  openIssues: Int,
  hasIssues: Boolean,
  hasWiki: Boolean,
  hasDownloads: Boolean
)

case class Commit(
  label: String,
  ref: String,
  sha: String,
  user: User,
  repo: Repo
)

case class PullReq(
  number: Int,
  state: String,
  title: String,
  body: String,
  user: User,
  head: Commit,
  base: Commit,
  createdAt: String,
  updatedAt: Option[String],
  closedAt: Option[String],
  mergedAt: Option[String],
  links: Map[String, String]
)

case class MergeResult(
  sha: String,
  merged: Boolean,
  message: String
)

object Gist {
  case class File(
    size: Int,
    tpe: String,
    language: String,
    rawUrl: String
  )
  case class Comment(
    id: Int,
    url: String,
    body: String,
    user: User,
    createdAt: String,
    updatedAt: String
  )
}

case class Gist(
  id: String,
  url: String,
  description: String,
  pub: Boolean,
  owner: User,
  comments: Int,
  files: Map[String, Gist.File]
)

case class GistDetails(
  id: String,
  url: String,
  description: String,
  pub: Boolean,
  owner: User,
  comments: Int,
  files: Map[String, Gist.File]
)

case class Hook(
  id: Int,
  url: String,
  createdAt: String,
  updatedAt: String,
  name: String,
  events: List[String],
  active: Boolean,
  config: Map[String, String]
)

case class Release(
  id: Int,
  url: String,
  tag: String,
  commitish: String,
  body: String,
  draft: Boolean,
  preprelease: Boolean,
  author: User
)

/** Type class for representing a response as a given type */
trait Rep[T] {
  def map: Response => T
}

object Rep {
  def str(name: String, fields: List[JField]): Option[String] =
    (for ((`name`, JString(value)) <- fields) yield value).headOption

  implicit val Identity: Rep[Response] = new Rep[Response] {
    def map = identity(_)
  }

  implicit val Nada: Rep[Unit] = new Rep[Unit] {
    def map = _ => ()
  }

  //implicit val Str: Rep[String] = new Rep[String] {
  //  def map = as.String
 //}

  implicit val OneAuthorization: Rep[Authorization] =
    new Rep[Authorization] {
      def map = as.json4s.Json andThen(AuthorizationList.one(_).get)
    }

  implicit object AuthorizationList extends Rep[List[Authorization]] {

    def one(js: JValue) = (for {
      JObject(auth)                      <- js
      ("id", JInt(id))                   <- auth
      ("url", JString(url))              <- auth
      ("scopes", JArray(scopes))         <- auth
      ("token", JString(token))          <- auth
      ("app", JObject(app))              <- auth
      ("note", JString(note))            <- auth
      ("note_url", JString(noteUrl))     <- auth
      ("created_at", JString(createdAt)) <- auth
      ("updated_at", JString(updatedAt)) <- auth
    } yield Authorization(
      id.toInt, url,
      for ( JString(scope) <- scopes) yield scope,
      token,
      (for {
        ("url", JString(appUrl))         <- app
        ("name", JString(appName))       <- app
        ("client_id", JString(clientId)) <- app
      } yield App(appUrl, appName, clientId)).head,
      note, noteUrl,
      createdAt, updatedAt)).headOption

    def map = as.json4s.Json andThen(for {
      JArray(auths) <- _
      auth          <- auths
    } yield one(auth).get)
  }

  implicit object Commits {
    def one(js: JValue) = (for {
      JObject(commit)           <- js
      ("label", JString(label)) <- commit
      ("ref", JString(ref))     <- commit
      ("sha", JString(sha))     <- commit
      ("user", user)            <- commit
      ("repo", repo)            <- commit
    } yield Commit(
      label, ref, sha,
      Users.one(user).get,
      Repos.one(repo).get)).headOption
  }

  implicit object Repos extends Rep[List[Repo]] {

    def map = as.json4s.Json andThen(for {
      JArray(repos) <- _
      repo          <- repos
    } yield one(repo).get)

    def one(js: JValue) = (for {
      JObject(repo)                           <- js
      ("id", JInt(id))                        <- repo
      ("owner", owner)                        <- repo
      ("name", JString(name))                 <- repo
      ("full_name", JString(fullName))        <- repo
      ("description", JString(desc))          <- repo
      ("private", JBool(priv))                <- repo
      ("fork", JBool(fork))                   <- repo
      ("url", JString(url))                   <- repo
      ("homepage", JString(home))             <- repo
      ("forks_count", JInt(forks))            <- repo
      ("stargazers_count", JInt(stars))       <- repo
      ("watchers_count", JInt(watches))       <- repo
      ("size", JInt(size))                    <- repo
      ("default_branch", JString(defBranch))  <- repo
      ("open_issues_count", JInt(openIssues)) <- repo
      ("has_issues", JBool(issues))           <- repo
      ("has_wiki", JBool(wiki))               <- repo
      ("has_downloads", JBool(dls))           <- repo
      ("pushed_at", JString(pushed))          <- repo
      ("created_at", JString(created))        <- repo
      ("updated_at", JString(updated))        <- repo
    } yield Repo(
      id.toInt, Users.one(owner).get,
      name, fullName, desc, priv, fork,
      url, home, forks.toInt, stars.toInt, watches.toInt, size.toInt,
      defBranch, openIssues.toInt, issues, wiki, dls)).headOption
  }

  implicit object Users {
    def one(js: JValue) = (for {
      JObject(user)             <- js
      ("login", JString(login)) <- user
      ("id", JInt(id))          <- user
      ("type", JString(typ))    <- user
      ("site_admin", JBool(siteAdmin)) <- user
    } yield User(login, id.toInt, typ, siteAdmin, Map.empty)).headOption
  }

  implicit object MergeResultDefaults extends Rep[MergeResult] {
    def map = { r =>
      (for {
        JObject(merge) <- as.json4s.Json(r)
        ("sha", JString(sha)) <- merge
        ("merged", JBool(merged)) <- merge
        ("message", JString(msg)) <- merge
      } yield MergeResult(sha, merged, msg)).head
    }
  }

  implicit object PullReqDetails extends Rep[PullReq] {
    def map = as.json4s.Json andThen(PullReqs.one(_).get)
  }

  implicit object PullReqs extends Rep[List[PullReq]] {
    def one(js: JValue) = (for {
      JObject(req)                     <- js
      ("number", JInt(num))            <- req
      ("state", JString(state))        <- req
      ("title", JString(title))        <- req
      ("body", JString(body))          <- req
      ("created_at", JString(created)) <- req
      ("user", user)                   <- req
      ("head", head)                   <- req
      ("base", base)                   <- req
      ("_links", JObject(links))       <- req
    } yield PullReq(
      num.toInt, state, title, body,
      Users.one(user).get,
      Commits.one(head).get,
      Commits.one(base).get,
      created,
      str("updated_at", req),
      str("closed_at", req),
      str("merged_at", req),
      (for {
        (name, JObject(link))   <- links
        ("href", JString(href)) <- link
      } yield (name, href)).toMap
    )).headOption

    def map = as.json4s.Json andThen(for {
      JArray(reqs) <- _
      req          <- reqs
    } yield one(req).get)
  }

  implicit object OneGist extends Rep[GistDetails] {
    def one(js: JValue) = (for {
      JObject(gist)                  <- js
      ("url", JString(url))          <- gist
      ("id", JString(id))            <- gist
      ("description", JString(desc)) <- gist
      ("public", JBool(pub))         <- gist
      ("owner", owner)               <- gist
      ("comments", JInt(comments))   <- gist
      ("files", JObject(files))      <- gist
    } yield GistDetails(
      id, url, desc, pub, Users.one(owner).get, comments.toInt, (for {
      (name, JObject(file))          <- files
    } yield (name, (for {
      ("size", JInt(size))        <- file
      ("raw_url", JString(url))   <- file
      ("type", JString(typ))      <- file
      ("language", JString(lang)) <- file
    } yield Gist.File(
      size.toInt, typ, lang, url)).head)).toMap)).headOption

    def map = as.json4s.Json andThen(one(_).get)
  }

  implicit object GistComments extends Rep[List[Gist.Comment]] {
    def one(js: JValue) = (for {
      JObject(com) <- js
      ("id", JInt(id)) <- com
      ("url", JString(url)) <- com
      ("body", JString(body))          <- com
      ("user", user)                   <- com
      ("created_at", JString(created)) <- com
      ("updated_at", JString(updated)) <- com
    } yield Gist.Comment(
      id.toInt, url, body, Users.one(user).get, created, updated)).headOption

    def map = as.json4s.Json andThen(for {
      JArray(comments) <- _
      comment          <- comments
    } yield one(comment).get)
  }

  implicit object GistComment extends Rep[Gist.Comment] {
    def map = as.json4s.Json andThen(GistComments.one(_).get)
  }

  implicit object Gists extends Rep[List[Gist]] {
    def one(js: JValue) = (for {
      JObject(gist)                  <- js
      ("url", JString(url))          <- gist
      ("id", JString(id))            <- gist
      ("description", JString(desc)) <- gist
      ("public", JBool(pub))         <- gist
      ("owner", owner)               <- gist
      ("comments", JInt(comments))   <- gist
      ("files", JObject(files))      <- gist
    } yield Gist(
      id, url, desc, pub, Users.one(owner).get, comments.toInt, (for {
      (name, JObject(file))          <- files
    } yield (name, (for {
      ("size", JInt(size))        <- file
      ("raw_url", JString(url))   <- file
      ("type", JString(typ))      <- file
      ("language", JString(lang)) <- file
    } yield Gist.File(
      size.toInt, typ, lang, url)).head)).toMap)).headOption

    def map = as.json4s.Json andThen(for {
      JArray(gists) <- _
      gist          <- gists
    } yield one(gist).get)
  }

  implicit object Hooks extends Rep[List[Hook]] {
    def one(js: JValue) = (for {
      JObject(hook)                    <- js
      ("id", JInt(id))                 <- hook
      ("url", JString(url))            <- hook
      ("created_at", JString(created)) <- hook
      ("updated_at", JString(updated)) <- hook
      ("name", JString(name))          <- hook
      ("events", JArray(events))       <- hook
      ("active", JBool(active))        <- hook
      ("config", JObject(config))      <- hook
    } yield Hook(
      id.toInt, url, created, updated, name, for {
        JString(ev) <- events
      } yield ev, active, (for {
        (name, JString(value)) <- config
      } yield (name, value)).toMap)).headOption

    def map = as.json4s.Json andThen(for {
      JArray(hooks) <- _
      hook          <- hooks
    } yield one(hook).get)
  }

  implicit object HookDetails extends Rep[Hook] {
    def map = as.json4s.Json andThen(Hooks.one(_).get)
  }

  implicit object Released extends Rep[Release] {
    def map = { r =>
      Releases.one(as.json4s.Json(r)).get
    }
  }

  implicit object Releases extends Rep[List[Release]] {
    def one(js: JValue) = (for {
      JObject(rel)                  <- js
      ("id", JInt(id))              <- rel
      ("url", JString(url))         <- rel
      ("tag_name", JString(tag))    <- rel
      ("target_commitish", JString(commitish)) <- rel
      ("body", JString(body))       <- rel
      ("draft", JBool(draft))       <- rel
      ("prerelease", JBool(prerel)) <- rel
      ("author", author)            <- rel
    } yield Release(
      id.toInt, url, tag, commitish, body, draft, prerel,
      Users.one(author).get
    )).headOption

    def map = as.json4s.Json andThen(for {
      JArray(rels) <- _
      rel          <- rels
    } yield one(rel).get)
  }
}
