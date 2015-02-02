package hubcat

import com.ning.http.client.Response
import java.io.File
import org.json4s._
import org.json4s.JsonDSL._

trait RepoReleases { self: RepoRequests =>

  object releases {

    private[this] def base =
      apiHost / "repos" / user / repo / "releases"

    /** https://developer.github.com/v3/repos/releases/#create-a-release
      * https://developer.github.com/v3/repos/releases/#edit-a-release */
    case class Builder(
      _id: Option[String]          = None,
      _tag: Option[String]         = None,
      _commitish: Option[String]   = None,
      _name: Option[String]        = None,
      _body: Option[String]        = None,
      _draft: Option[Boolean]      = None,
      _prerelease: Option[Boolean] = None)
      extends Client.Completion[Release] {

      def tag(t: String) = copy(_tag = Some(t))

      def commitish(c: String) = copy(_commitish = Some(c))

      def name(n: String) = copy(_name = Some(n))

      def body(b: String) = copy(_body = Some(b))

      def draft(d: Boolean) = copy(_draft = Some(d))

      def prerelease(p: Boolean) = copy(_prerelease = Some(p))

      def apply[T](hand: Client.Handler[T]) =
        request(_id.fold(base.POST)(base.PATCH / _)
                << body)(hand)

      def body = json.str(
        ("tag_name"         -> _tag) ~
        ("target_commitish" -> _commitish) ~
        ("name"             -> _name) ~
        ("body"             -> _body) ~
        ("draft"            -> _draft) ~
        ("prerelease"       -> _prerelease))
    }

    case class Assets(rel: String)
      extends Client.Completion[Response] {
      private[this] def assetBase = base / rel / "assets"

      case class Edit(
        id: String,
        _name: Option[String]  = None,
        _label: Option[String] = None)
        extends Client.Completion[Response] {

        def name(n: String) = copy(_name = Some(n))

        def label(l: String) = copy(_label = Some(l))

        def apply[T](hand: Client.Handler[T]) =
          request(assetBase.PATCH / id
                  << body)(hand)

        def body = json.str(
          ("name"  -> _name) ~
          ("label" -> _label))
      }

      /** https://developer.github.com/v3/repos/releases/#list-assets-for-a-release */
      def apply[T](hand: Client.Handler[T]) =
        request(assetBase)(hand)

      def upload(name: String, file: File) =
        complete[Response](assetBase <<? Map("name" -> name))

      def get(id: String) =
        complete[Response](assetBase / id)

      def edit(id: String) = Edit(id)

      def delete(id: String) =
        complete[Response](assetBase.DELETE / id)
    }

    def list = complete[List[Release]](base)

    /** https://developer.github.com/v3/repos/releases/#get-a-single-release */
    def get(id: String) = complete[Release](base / id)

    /** https://developer.github.com/v3/repos/releases/#create-a-release */
    def create = Builder()

    /** https://developer.github.com/v3/repos/releases/#edit-a-release */
    def edit(id: String) = Builder(Some(id))

    def delete(id: String) = complete[Unit](base.DELETE / id)

    /** https://developer.github.com/v3/repos/releases/#list-assets-for-a-release */
    def assets(id: String) = Assets(id)
  }
}
