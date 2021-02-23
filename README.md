# JSON Schema to AVRO Schema Converter

Converts JSON Schema Draft 6 into Avro 1.10.1

These versions were chosen by the specific need to convert the FHIR v4 schema.

## Specifications

- [JSON Schema Draft 6](https://tools.ietf.org/html/draft-wright-json-schema-01)
- [JSON Schema Draft 6 Validation](https://tools.ietf.org/html/draft-wright-json-schema-validation-01)
  * The validation document actually has quite a lot of information about the actual schema 
    that isn't in the main RFC
- [JSON Schema Draft 6 as a JSON Schema document](https://json-schema.org/draft-06/schema)
- [Avro Specification v 1.10.1](https://avro.apache.org/docs/1.10.1/spec.html)

## Developing

- [Install `sbt` and Scala](https://docs.scala-lang.org/getting-started/index.html)

```console
sbt test
```

```console
sbt "run [--namespace "com.example"] path/to/inputFile"
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