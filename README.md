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