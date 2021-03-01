package io.carrera.jsontoavroschema

object AvroWriter {

  def toJson(record: AvroRecord): ujson.Obj = {
    val result =
      ujson.Obj(
        "type" -> "record",
        "name" -> record.name
      )
    record.namespace.foreach(ns => result("namespace") = ns)
    record.doc.foreach(d => result("doc") = d)
    result("fields") = record.fields.map(toJson)
    result
  }

  private def toJson(field: AvroField): ujson.Obj = {
    val result =
      ujson.Obj("name" -> field.name)
    field.doc.foreach(d => result("doc") = d)
    result("type") = toJson(field.`type`)
    result
  }

  private def toJson(avroType: AvroType): ujson.Value =
    avroType match {
      case AvroString => "string"
      case AvroDouble => "double"
      case AvroNull => "null"
      case AvroBool => "boolean"
      case AvroLong => "long"
      case AvroBytes => "bytes"
      case AvroArray(t) =>
        ujson.Obj("type" -> "array", "items" -> toJson(t))
      case AvroMap(t) =>
        ujson.Obj("type" -> "map", "values" -> toJson(t))
      case AvroEnum(name, symbols) =>
        ujson.Obj("type" -> "enum", "name" -> name, "symbols" -> symbols)
      case AvroUnion(types) => types.map(toJson)
      case r: AvroRecord =>
        toJson(r)
    }
}
