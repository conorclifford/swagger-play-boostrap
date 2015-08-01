# swagger-play-boostrap

**The Wrong way to use Swagger within [Play](http://www.playframework.com).**

(hence the intentional typo in project name)

A very simple (rough, incomplete, etc.) tool to bootstrap some useful files for a Play application from a swagger.yaml file.

This is most likely the wrong way to integrate API-first (using Swagger) design into Play.

# Usage

```
$ sbt assembly
$ scala target/scala-2.11/swagger-play-boostrap-assembly-0.1-SNAPSHOT.jar /path/to/swagger.yaml /path/to/generate/output
```

## Notes

* path for generated output is created, and must not already exist (support for merging into existing code is not supported)
* there is limited support for types of parameters, etc. (incomplete code in this tool currently)
* (Very) Incomplete support for Swagger specification - tool still WIP, (mostly) works for some simple test APIs run through it)
