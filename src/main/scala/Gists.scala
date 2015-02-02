package hubcat

import dispatch.Req
import java.util.Date
import org.json4s.JsonDSL._

trait Gists { self: Requests =>

  protected [this]
  case class GistBuilder(
    _files: Map[String, String] = Map.empty,
    _desc: Option[String]       = None,
    _vis: Boolean               = true)
    extends Client.Completion[GistDetails] {

    def desc(d: String) = copy(_desc = Some(d))
    def pub = copy(_vis = true)
    def secret = copy(_vis = false)
    def file(content: String, name: String = s"f${_files.size}") =
      copy(_files = _files + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "gists" << body)(handler)

    def body = json.str(
      ("public"      -> _vis) ~
      ("description" -> _desc) ~
      ("files"       -> _files.map {
        case (name, content) =>
          (name -> ("content" -> content))
      }))
  }

  protected [this]
  case class RegistBuilder(
    id: String,
    _files: Map[String, String] = Map.empty,
    _desc: Option[String]       = None)
    extends Client.Completion[GistDetails] {

    def desc(d: String) = copy(_desc = Some(d))
    def file(content: String, name: String = s"f{_files.size}") =
      copy(_files = _files + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.PATCH / "gists" / id << body)(handler)

    def body = json.str(
      ("description" -> _desc) ~
      ("files"       -> _files.map {
        case (name, content) => (name -> ("content" -> content))
      }))
  }

   object gists {
    case class GistLimiter(
      base: Req,
      sinceval: Option[String] = None)
      extends Client.Completion[List[hubcat.Gist]] {
      def since(d: Date) = copy(sinceval = Some(ISO8601(d)))
      override def apply[T]
       (handler: Client.Handler[T]) =
        request(base <<? Map.empty[String,String] ++ sinceval.map("since" -> _))(handler)
    }

     /** http://developer.github.com/v3/gists/#list-gists */
    def user(user: String) =
      GistLimiter(apiHost / "users" / user / "gists")

    def owned =
      GistLimiter(apiHost / "gists")

    def everyone =
      GistLimiter(apiHost / "gists" / "public")

    def starred =
      complete[List[hubcat.Gist]](apiHost / "gists" / "starred")

    protected [this]
    class Gist(id: String)
     extends Client.Completion[GistDetails] {

       protected [this]
       case class Comment(
         cid: Int, accept: String = Accept.GithubJson)
         extends Client.Completion[hubcat.Gist.Comment] {

           def accepting = new {
             def raw = copy(accept = Accept.RawJson)
             def text = copy(accept = Accept.TextJson)
             def html = copy(accept = Accept.HtmlJson)
             def fullJson = copy(accept = Accept.FullJson)
           }

           /** http://developer.github.com/v3/gists/comments/#delete-a-comment */
           def delete =
             complete[Unit](apiHost.DELETE / "gists" / id / "comments" / cid.toString)

           /** http://developer.github.com/v3/gists/comments/#edit-a-comment */
           def edit(body: String) =
             complete[hubcat.Gist.Comment](apiHost.PATCH / "gists" / id / "comments" / cid.toString
                                           << json.str(("body" -> body)))

           /** http://developer.github.com/v3/gists/comments/#get-a-single-comment */
           override def apply[T](hand: Client.Handler[T]) =
             request(apiHost / "gists" / id / "comments" / cid.toString <:< Map("Accept" -> accept))(hand)
       }
       
       /** http://developer.github.com/v3/gists/comments/#list-comments-on-a-gist */
       def comments =
         complete[List[hubcat.Gist.Comment]](apiHost / "gists" / id / "comments")

       def comment(cid: Int) =
         Comment(cid)

       /** http://developer.github.com/v3/gists/comments/#create-a-comment */
       def comment(body: String) =
         complete[hubcat.Gist.Comment](apiHost.POST / "gists" / id / "comments"
                                       << json.str(("body" -> body)))
         
       /** http://developer.github.com/v3/gists/#get-a-single-gist */
       override def apply[T](hand: Client.Handler[T]) =
         request(apiHost / "gists" / id)(hand)
    }
    
    def get(id: String) =
      new Gist(id)

    /** http://developer.github.com/v3/gists/#create-a-gist */
    def post =
      GistBuilder()

    /** http://developer.github.com/v3/gists/#edit-a-gist */
    def regist(id: String) =
      RegistBuilder(id)

    /** http://developer.github.com/v3/gists/#star-a-gist */
    def star(id: String) = // 411 length required
      complete[Unit](apiHost.PUT / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#unstar-a-gist */
    def unstar(id: String) = 
      complete[Unit](apiHost.DELETE / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#check-if-a-gist-is-starred */
    def starred(id: String) =
      GistLimiter(apiHost.GET / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#fork-a-gist */
    def fork(id: String) = 
      complete[Unit](apiHost.POST / "gists" / id / "forks")

    def forks(id: String) =
      complete[List[hubcat.Gist]](apiHost / "gists" / id / "forks")

    /** http://developer.github.com/v3/gists/#delete-a-gist */
    def delete(id: String) = 
      complete[Unit](apiHost.DELETE / "gists" / id)
   }
}
