package hubcat

import hubcat._; import dispatch._; import org.json4s._
import org.scalatest.FunSpec 

class ClientSpec extends FunSpec {
  describe ("Authorization Client") {
    val Seq(user, pass) = Seq("GHUSER", "GHPASS")
                          .map(p => Option(System.getProperty(p)).getOrElse(sys.error("missing %s".format(p))))
    val TestNote = "imaunittest"
    val cli = AuthorizationClient(user, pass)

    it ("should authorize tokens") {
      val auth = cli.authorize
                    .scopes("repo")
                    .note(TestNote)
      val token = for {
        JObject(fields)           <- auth(as.json4s.Json)()
        ("token", JString(token)) <- fields
      } yield token
      assert(1 === token.size)
    }

    it ("should list authorizations") {
      val auths = for {
        JArray(ary) <- cli.authorizations(as.json4s.Json)()        
        JObject(fs) <- ary
        ("id", JInt(id)) <- fs
      } yield id
      assert(!auths.isEmpty)
      assert(auths.isEmpty == false)
    }

    it ("should find an authorizaton by id") {
      val auths = for {
        JArray(ary) <- cli.authorizations(as.json4s.Json)()
        JObject(fs) <- ary
        ("id", JInt(id)) <- fs
      } yield id
      assert(!auths.isEmpty)
      assert(auths.forall( thisId => (for {
        JObject(fs) <- cli.authorization(thisId.toInt)(as.json4s.Json)()
        ("id", JInt(thatId)) <- fs
      } yield thatId == thisId).head))
    }
    
    it ("should deauthorize tokens") {
      val auths = for {
        JArray(ary) <- cli.authorizations(as.json4s.Json)()
        JObject(fs) <- ary
        ("id", JInt(id)) <- fs
        ("note", JString(note)) <-fs
        if (note == TestNote)
      } yield id
      assert(!auths.isEmpty)
      auths.map( id => cli.deauthorize(id.toInt)(as.String)())
      assert(auths.forall( id => (for {
        JObject(fs) <- cli.authorization(id.toInt)(as.json4s.Json)()
        ("message", JString(msg)) <- fs
      } yield msg == "Not Found").head))
    }
  }
}
