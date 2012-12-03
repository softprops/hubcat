package hubcat

import dispatch._
import com.ning.http.client.RequestBuilder

trait Hosts {
  def apiHost: RequestBuilder
}

trait DefaultHosts extends Hosts {
  import Types.GithubJson
  def apiHost = :/("api.github.com").secure <:< Map("Content-Type" -> GithubJson)
}
