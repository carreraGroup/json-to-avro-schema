# Type Mapping

`*` indicates a complex type

| JSON    | AVRO          | Notes                                                                            |
|---------|---------------|----------------------------------------------------------------------------------|
| null    | null          |                                                                                  |
| boolean | boolean       |                                                                                  |
| n/a     | int           | No JSON Schema type maps onto an AVRO type                                       |
| integer | long          | [JSON is capable of 53 bit ints][jsonspec]                                       |
| number  | double        | [JSON spec][jsonspec] recommends doubles                                         |
| string  | string        |                                                                                  |
| any     | bytes         | JSON schema allows for "any" types indicated by an empty schema or missing field |
| array   | array*        | *avro complex type, requires knowing the item type                               |
| object* | record        | if an `$id` is present                                                           |
| object* | map           | if an `$id` is not present                                                       |
| enum*   | ???           | JSchema allows enum values to be `any`, but AVRO expects a list of strings       |

<!-- References -->
[jsonspec]: https://tools.ietf.org/html/rfc7159#section-6