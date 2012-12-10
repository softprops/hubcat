# hubcat

hip cats, git hubs, good times

## usage 

Get a list of contributors for repo ranked by contribution

```scala
import hubcat._;import dispatch._;import net.liftweb.json._
val (login, pass) = ("yrlogin", "yrpass")
val (user, repo) = ("dispatch", "reboot")
val auth = new AuthorizationClient(login, pass).
                 authorize.
                 scopes("repo").
                 note("just for fun")
for {
  JObject(fields)   <- auth(as.lift.Json)()
  JField("token", JString(token)) <- fields
} yield {
  val contrib = new Client(token).
                     contributors(user, repo)
  (for {
    JObject(cfields)     <- contrib(as.lift.Json)()
    JField("login", JString(login))      <- cfields
    JField("contributions", JInt(count)) <- cfields
  } yield (login, count)).sortBy(_._2).reverse
}.foreach {
  case (l, c) => println("%-15s - %s" format(l, c))
}
```

Doug Tangren (softprops) 2012
