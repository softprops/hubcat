package hubcat

import dispatch.{ :/, Req }

trait Hosts {
  def apiHost: Req
}

trait DefaultHosts extends Hosts {
  def apiHost = :/("api.github.com").secure <:< Map("Content-Type" -> Accept.GithubJson)
}
