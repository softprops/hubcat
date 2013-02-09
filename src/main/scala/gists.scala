package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder
import java.util.Date

trait Gists { self: Requests =>

  protected [this]
  case class GistBuilder(filevals: Map[String, String] = Map.empty[String, String],
                         descval: Option[String] = None,
                         vis: Boolean = true)
     extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(descval = Some(d))
    def pub = copy(vis = true)
    def secret = copy(vis = false)
    def file(content: String, name: String = "f%s" format filevals.size) =
      copy(filevals = filevals + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.POST / "gists" << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js =
        ("public" -> vis) ~ 
        ("description" -> jStringOrNone(descval)) ~
        ("files" -> filevals.map {
          case (name, content) => (name -> ("content" -> content))
        })
      compact(render(js))
    }
  }

  protected [this]
  case class RegistBuilder(id: String,
                           filevals: Map[String, String] = Map.empty[String, String],
                           descval: Option[String] = None)
     extends Client.Completion
        with Jsonizing {

    def desc(d: String) = copy(descval = Some(d))
    def file(content: String, name: String = "f%s" format filevals.size) =
      copy(filevals = filevals + ((name, content)))

    override def apply[T](handler: Client.Handler[T]) =
      request(apiHost.PATCH / "gists" / id << pjson)(handler)

    private def pjson = {
      import org.json4s.JsonDSL._
      import org.json4s.native.Printer.compact
      import org.json4s.native.JsonMethods.render
      val js =
        ("description" -> jStringOrNone(descval)) ~
        ("files" -> filevals.map {
          case (name, content) => (name -> ("content" -> content))
        })
      compact(render(js))
    }
  }

   protected [this]
   object GistMethods {
    case class GistLimiter(base: RequestBuilder, sinceval: Option[String] = None)
    extends Client.Completion {
      def since(d: Date) = copy(sinceval = Some(ISO8601(d)))
      override def apply[T](handler: Client.Handler[T]) =
        request(base <<? Map.empty[String,String]++sinceval.map("since" -> _))(handler)
    }

     /** http://developer.github.com/v3/gists/#list-gists */
    def user(user: String) =
      GistLimiter(apiHost / "users" / user / "gists")

    def owned =
      GistLimiter(apiHost / "gists")

    def starred =
      complete(apiHost / "gists" / "starred")

    def everyone =
      GistLimiter(apiHost / "gists" / "public")

    protected [this]
    class Gist(id: String)
     extends Client.Completion {

       protected [this]
       case class Comment(cid: Int, accept: String = Types.GithubJson)
         extends Client.Completion {

           def accepting = new {
             def raw = copy(accept = Types.RawJson)
             def text = copy(accept = Types.TextJson)
             def html = copy(accept = Types.HtmlJson)
             def fullJson = copy(accept = Types.FullJson)
           }

           /** http://developer.github.com/v3/gists/comments/#delete-a-comment */
           def delete =
             complete(apiHost.DELETE / "gists" / id / "comments" / cid.toString)

           /** http://developer.github.com/v3/gists/comments/#edit-a-comment */
           def edit(body: String) = {
             import org.json4s.JsonDSL._
             import org.json4s.native.Printer.compact
             import org.json4s.native.JsonMethods.render
             complete(apiHost.PATCH / "gists" / id / "comments" / cid.toString << compact(render(("body" -> body))))
           }

           /** http://developer.github.com/v3/gists/comments/#get-a-single-comment */
           override def apply[T](hand: Client.Handler[T]) =
             request(apiHost / "gists" / id / "comments" / cid.toString <:< Map("Accept" -> accept))(hand)
       }
       
       /** http://developer.github.com/v3/gists/comments/#list-comments-on-a-gist */
       def comments =
         complete(apiHost / "gists" / id / "comments")

       def comment(cid: Int) =
         Comment(cid)

       /** http://developer.github.com/v3/gists/comments/#create-a-comment */
       def comment(body: String) = {
         import org.json4s.JsonDSL._
         import org.json4s.native.Printer.compact
         import org.json4s.native.JsonMethods.render
         complete(apiHost.POST / "gists" / id / "comments" << compact(render(("body" -> body))))
       }
         
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
      complete(apiHost.PUT / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#unstar-a-gist */
    def unstar(id: String) = 
      complete(apiHost.DELETE / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#check-if-a-gist-is-starred */
    def starred(id: String) =
      GistLimiter(apiHost.GET / "gists" / id / "star")

    /** http://developer.github.com/v3/gists/#fork-a-gist */
    def fork(id: String) = 
      complete(apiHost.POST / "gists" / id / "fork")

    /** http://developer.github.com/v3/gists/#delete-a-gist */
    def delete(id: String) = 
      complete(apiHost.DELETE / "gists" / id)
   }

  def gists = GistMethods
}
