# hubcat

hip cats, git hubs, good times.

## install

In sbt...

    libraryDependencies += "me.lessis" %% "hubcat" % "0.1.0"

## goals

- do as __little__ as possible while providing much of what you __need__.

## what it does

- handle authentication (basic & oauth)

Most HTTP APIs are composed of REST-style endpoints which have little need for API wrappers. Dispatch's path building interface is sufficient and down-right ideal to suit most of these cases. Authenticated APIs incur some extra complexity for clients which makes using a library more worth your while to avoid writing extra code for handling API specific authentication. This library provides an interface that supports github basic and oauth(2) credentials. How you choose to store those credentials is up to your application.

- provide a fluent interface for composing api requests

The choice of design is modeled after the library dispatch is based on which provides low-level builder interface for composing HTTP requests. This library provides a builder interface at the level of the github API.

## what it doesn't

- make assumptions about what you want to do with responses

In the spirit of dispatch, your interface for responses is a just function. It just happens to have
the same function signature dispatch uses for handlers of responses: `(com.ning.http.client.Response => T)`

All builder methods define a method `apply[T](hand: Client.Handler[T]): Promise[T]` so, at any point,
you can provide a handler function and get back a dispatch Promise to compose with other promises to process a chain of responses.

- provide a modeling of responses

Many applications may already define their own representation of github api objects.
This library does not intend to replace those. This library allows you to directly adapt them to standard
dispatch interfaces.

- parse responses

Most of the time you will wish to query a github server for a single piece of information. Github serves most services in JSON.
There are alot of great JSON parsers out there. You are probably already using one. This library doesn't assume which one you will use.
There is probably already a dispatch interface out there for the one you are using. You should use those instead. If not, contribute to dispatch!

## usage 

Get a list of contributors for repo ranked by contribution

```scala
import hubcat._; import dispatch._; import net.liftweb.json._
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

### Todo

(write some tests already)

### Resources

- Github [api docs](http://developer.github.com/)
- Your Github [oauth clients](https://github.com/settings/applications)

Doug Tangren (softprops) 2012
