# Roadmap

There are several major milestones for this project.

- [x] Simplest possible schema
- [ ] Transpile the JSON Schema meta schema
- [ ] Transpile the FHIR schema
- [ ] Everything else?

The goal is to be a general purpose tool, but we're prioritizing the keywords required for transpiling the FHIR schema because we have an immediate need for it.

Below is the list of JSON schema keywords and their implementation status.

| Keyword              | Status  | FHIR Uses | Notes                                                                               | Specification URL                                                               |
|----------------------|---------|-----------|-------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| $id                  |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-01#section-9.2             |
| $schema              |         | Y         | In the future, we should use this to determine how to parse                         | https://tools.ietf.org/html/draft-wright-json-schema-01#section-7               |
| $ref                 |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-01#section-8               |
| definitions          |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.1  |
| title                |         | Y         | Could go in AVRO `doc` field with description                                       | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.2  |
| description          | DONE    | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.2  |
| default              |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.3  |
| examples             |         | N         | Could go in AVRO `doc` field with description                                       | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-7.4  |
| multipleOf           |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.1  |
| maximum              |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.2  |
| exclusiveMaximum     |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.3  |
| minimum              |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.4  |
| exclusiveMinimum     |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.5  |
| maxLength            |         | N         | FHIR defines it's own maxLength                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.6  |
| minLength            |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.7  |
| pattern              |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.8  |
| items                |         | Y         | FHIR typically uses a $ref for the type                                             | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.9  |
| additionalItems      |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.10 |
| maxItems             |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.11 |
| minItems             |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.12 |
| uniqueItems          |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.13 |
| contains             |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.14 |
| maxProperties        |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.15 |
| minProperties        |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.16 |
| required             |         | Y         | If not required, default should be null  and null should be first in the type union | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.17 |
| properties           | DONE    | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.18 |
| patternProperties    |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.19 |
| additionalProperties |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.20 |
| dependencies         |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.21 |
| propertyNames        |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.22 |
| enum                 |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.23 |
| const                |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.24 |
| type                 | Partial | Y         | Doesn't allow for unions yet                                                        | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.25 |
| allOf                |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.26 |
| anyOf                |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.27 |
| oneOf                |         | Y         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.28 |
| not                  |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-6.29 |
| format               |         | N         |                                                                                     | https://tools.ietf.org/html/draft-wright-json-schema-validation-01#section-8    |