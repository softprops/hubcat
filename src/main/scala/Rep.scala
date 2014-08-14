package hubcat

import com.ning.http.client.Response
import dispatch.as
import org.json4s._
import org.json4s.JsonDSL._

case class PullReq(
  number: Int,
  state: String,
  title: String,
  body: String,
  createdAt: String,
  updatedAt: Option[String],
  closedAt: Option[String],
  mergedAt: Option[String],
  links: Map[String, String]
)

sealed trait Rep[T] {
  def map: Response => T
}

object Rep {
  implicit object Identity extends Rep[Response] {
    def map = identity(_)
  }

  implicit object Nada extends Rep[Unit] {
    def map = _ => ()
  }

  implicit object PullReqs extends Rep[List[PullReq]] {
    def map = as.json4s.Json andThen(for {
      JArray(reqs)                     <- _
      JObject(req)                     <- reqs
      ("number", JInt(num))            <- req
      ("state", JString(state))        <- req
      ("title", JString(title))        <- req
      ("body", JString(body))          <- req
      ("created_at", JString(created)) <- req
      ("_links", JObject(links))       <- req
    } yield PullReq(
      num.toInt, state, title, body, created,
      (for ( ("updated_at", JString(updated)) <- req ) yield updated).headOption,
      (for ( ("closed_at", JString(closed)) <- req ) yield closed).headOption,
      (for ( ("merged_at", JString(merged)) <- req ) yield merged).headOption,
      (for {
        (name, JObject(link))   <- links
        ("href", JString(href)) <- link
      } yield (name, href)).toMap
    ))
  }
}
