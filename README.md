# swagger-play-boostrap

**The Wrong way to use Swagger within [Play](http://www.playframework.com).**

(hence the intentional typo in project name)

A very simple (rough, incomplete, etc.) tool to bootstrap some useful files for a Play application from a swagger.yaml file.

This is most likely the wrong way to integrate API-first (using Swagger) design into Play.

# Usage

## Service bootstrapping

```
$ sbt assembly
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output
```

## Client Generation

use a trailing `-client` flag to generate client code instead:

```
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output -client
```

This generates client code similar to:

```
/path/to/generate/output/
└── clients
    └── lower_case_service_name
        ├── Client.scala
        ├── JsonOps.scala
        └── Models.scala
```

* `Models.scala` contains generated `case class` instances for Swagger definitions.
* `JsonOps.scala` contains `implicit` [Play-Json](https://www.playframework.com/documentation/2.4.x/ScalaJson#The-Play-JSON-library) `Reads` and `Writes` (i.e. JSON marshalling/unmarshalling implicits)
* `Client.scala` contains generated client abstractions. These clients use [Play-WS](https://www.playframework.com/documentation/2.4.x/ScalaWS) to interact with the remote REST service in a non-blocking manner. Note, this generated code also has a dependency on [Scalaz](https://github.com/scalaz/scalaz), e.g.

```
libraryDendenencies += "org.scalaz" %% "scalaz-core" % "7.1.3"
```

### Using this client external to a Play application

While this client uses Play-WS to do REST calls, and it is configured by default to use the existing/running application for these WS interactions, it is possible to use this client in a non-Play application.

This can be done by creating, and providing, a separate `play.api.libs.ws.WSClient` when constructing the instance of the `Client`.

for example,

```scala
val wsClient = {
	val builder = new com.ning.http.client.AsyncHttpClientConfig.Builder()
	new play.api.libs.ws.ning.NingWSClient(builder.build())
}

val client = new Client("http://localhost:9000", wsClient = wsClient)
```

## Notes

* path for generated output is created, and must not already exist (support for merging into existing code is not supported)
* there is limited support for types of parameters, etc. (incomplete code in this tool currently)
* (Very) Incomplete support for Swagger specification - tool still WIP, (mostly) works for some simple test APIs run through it)
