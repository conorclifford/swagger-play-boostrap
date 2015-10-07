# swagger-play-boostrap

**The Wrong way to use Swagger within [Play](http://www.playframework.com).**

(hence the intentional typo in project name)

A very simple (rough, incomplete, etc.) tool to bootstrap some useful files for a Play application from a swagger.yaml file.

This is most likely the wrong way to integrate API-first (using Swagger) design into Play.

# Usage

## Service bootstrapping

Generation of code routes, controllers, `case class`es for all Definitions, with supporting Play-json `Reads` and `Writes` implicits, for a provided swagger API definition (in either JSON or YAML).

**This code is generated for Play 2.4**

```
$ sbt assembly
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output
```

## Client Generation

This tool can also generate client code, which includes Clients to interact with resources, `case class`es for all Definitions, with supporting Play-json `Reads` and `Writes` implicits.

**This generated code is built for, and tested with, Play 2.4**. Unfortunately, this includes when using this generated client separately to a Play app (see below), due to various API signature changes in 2.4.

use a trailing `-client` flag to generate client code instead:

```
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output -client
```

use a trailing `-replace` flag to generate code in-place over existing generated code - this will remove existing directory for the API being generated for, and recreate new code in its place (this is only supported for *client* code generation):

```
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output -client -replace
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
* `JsonOps.scala` contains `implicit` [Play-Json (2.4)](https://www.playframework.com/documentation/2.4.x/ScalaJson#The-Play-JSON-library) `Reads` and `Writes` (i.e. JSON marshalling/unmarshalling implicits)
* `Client.scala` contains generated client abstractions. These clients use [Play-WS (2.4)](https://www.playframework.com/documentation/2.4.x/ScalaWS) to interact with the remote REST service in a non-blocking manner. Note, this generated code also has a dependency on [Scalaz](https://github.com/scalaz/scalaz), e.g.

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
