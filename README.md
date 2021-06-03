# JSON Schema to AVRO Schema Converter

Converts JSON Schema Draft 6 into Avro 1.10.1

These versions were chosen by the specific need to convert the FHIR v4 schema.

## Quickstart

See [packaging](#packaging) and [run](#running)

To understand how JSON Schema types are mapped onto AVRO types, see [Type Mapping][type-mapping].

To see the current implementation status, see the [Roadmap][roadmap].

## Specifications

- [JSON Schema Draft 6][json-schema-spec]
- [JSON Schema Draft 6 Validation][json-schema-validation-spec]
  * The validation document actually has quite a lot of information about the actual schema 
    that isn't in the main RFC
- [JSON Schema Draft 6 as a JSON Schema document][meta-schema]
- [Avro Specification v 1.10.1][avro-spec]

## Developing

- [Install `sbt` and Scala][install-scala]

As features are added, they should be added to the [integration-test][integration-test-dir] directory,
the [integration-test][integration-test], run through the tool, 
and compiled with `avro-tools` to verify we're generating correct AVRO schemas.  

```console
avro-tools compile schema src/test/resources/integration-tests/*.avsc output/
```

The [keyword matrix][roadmap] should also be updated.

### Test

```console
sbt test
```

### Debug

You can run the app from a `sbt` console like this.

```console
run [--namespace "com.example"] path/to/inputFile path/to/outputDirectory
```

You can also do it from the command line by wrapping the command in quotes.

```console
sbt "run [--namespace "com.example"] path/to/inputFile path/to/outputDirectory"
```

### Packaging

We use the [sbt assembly plugin][sbt-assembly] to generate a fat jar.

```console
sbt assembly
```

Which can then be run with `java` or `scala`.

### Running

```console
$ java -jar target/scala-2.13/json-to-avro-schema-assembly-0.1.jar src/test/resources/simple-schema.json path/to/outputDirectory
input loaded
parsed
success
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

The decision was made to output a single `*.avsc` file per definition in the JSON schema
because of limitations in the AVRO tooling. 
It doesn't handle large `*.avsc` files well even though we can correctly generate very large schemas.
This does come at the cost of not being able to process JSON schemas with circular references very well.

<!-- References -->
[roadmap]: ./docs/Roadmap.md
[type-mapping]: ./docs/TypeMappings.md
[integration-test-dir]: ./src/test/resources/integration-tests
[integration-test]: ./src/test/scala/IntegrationTests.scala

[json-schema-spec]: https://tools.ietf.org/html/draft-wright-json-schema-01
[json-schema-validation-spec]: https://tools.ietf.org/html/draft-wright-json-schema-validation-01
[meta-schema]: https://json-schema.org/draft-06/schema
[avro-spec]: https://avro.apache.org/docs/1.10.1/spec.html

[install-scala]: https://docs.scala-lang.org/getting-started/index.html
[sbt-assembly]: https://github.com/sbt/sbt-assembly