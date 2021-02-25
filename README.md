# JSON Schema to AVRO Schema Converter

Converts JSON Schema Draft 6 into Avro 1.10.1

These versions were chosen by the specific need to convert the FHIR v4 schema.

## Quickstart

See [packaging](#packaging) and [run](#running)

To understand how JSON Schema types are mapped onto AVRO types, see [Type Mapping][type-mapping].

To see the current impelementation status, see the [Roadmap][roadmap].

## Specifications

- [JSON Schema Draft 6][json-schema-spec]
- [JSON Schema Draft 6 Validation][json-schema-validation-spec]
  * The validation document actually has quite a lot of information about the actual schema 
    that isn't in the main RFC
- [JSON Schema Draft 6 as a JSON Schema document][meta-schema]
- [Avro Specification v 1.10.1][avro-spec]

## Developing

- [Install `sbt` and Scala][install-scala]

As features are added, they should be added to [integration-test.json][integration-test],
run through the tool, and compiled with `avro-tools` to verify we're generating correct AVRO schemas.  
The [keyword matrix][roadmap] should also be updated.

### Test

```console
sbt test
```

### Debug

You can run the app from a `sbt` console like this.

```console
run [--namespace "com.example"] path/to/inputFile 
```

You can also do it from the command line by wrapping the command in quotes.

```console
sbt "run [--namespace "com.example"] path/to/inputFile"
```

However, the `sbt` output also goes to stdout, so this isn't appropriate for actual generation.

### Packaging

We use the [sbt assembly plugin][sbt-assembly] to generate a fat jar.

```console
sbt assembly
```

Which can then be run with `java` or `scala`.

### Running

```console
$ java -jar target/scala-2.13/json-to-avro-schema-assembly-0.1.jar src/test/resources/simple-schema.json
input loaded
parsed
success
{
  "type": "record",
  "name": "schema",
  "fields": [
    {
      "name": "title",
      "type": "string"
    }
  ]
}
```

`stdout` can be redirected to a file.

```console
$ java -jar target/scala-2.13/json-to-avro-schema-assembly-0.1.jar src/test/resources/simple-schema.json > success.avsc
input loaded
parsed
success
$ ls
success.avsc
```

## Architectural considerations

This process is run as a source generation step and is therefore a build time concern, not a runtime one.
This means that a solution need not be _fast_, only correct.
Of course, we also don't want to be so slow that regenerating is a painful experience to be avoided.

Being a build time concern also means a solution does not _necessarily_ need to run on the JVM.
However, since the team building this spends most of their time developing in Scala,
we chose the same implementation language.

Long term, it would be nice to also generate a FHIR to Avro mapper along with the Avro schema definitions.
Otherwise, we will need to continue manually adjusting the wrapper whenever we change the way the schemas are generated.
We should do our best to allow for this to be easily implemented later.

<!-- References -->
[roadmap]: ./docs/Roadmap.md
[type-mapping]: ./docs/TypeMappings.md
[integration-test]: ./src/test/resources/integration-test.json

[json-schema-spec]: https://tools.ietf.org/html/draft-wright-json-schema-01
[json-schema-validation-spec]: https://tools.ietf.org/html/draft-wright-json-schema-validation-01
[meta-schema]: https://json-schema.org/draft-06/schema
[avro-spec]: https://avro.apache.org/docs/1.10.1/spec.html

[install-scala]: https://docs.scala-lang.org/getting-started/index.html
[sbt-assembly]: https://github.com/sbt/sbt-assembly