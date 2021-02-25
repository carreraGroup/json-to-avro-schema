package io.carrera.jsontoavroschema

object AvroWriter {

  def toJson(record: AvroRecord): Either[TranspileError, ujson.Obj] = {
    val result =
      ujson.Obj(
        "type" -> "record",
        "name" -> record.name
      )
    record.namespace.foreach(ns => result("namespace") = ns)
    record.doc.foreach(d => result("doc") = d)
    result("fields") = record.fields.map(toJson)
    Right(result)
  }

  private def toJson(field: AvroField): ujson.Obj = {
    val result =
      ujson.Obj("name" -> field.name)
    field.doc.foreach(d => result("doc") = d)
    result("type") = toJson(field.`type`)
    result
  }

  private def toJson(`type`: AvroType) =
    `type` match {
      case c @ AvroArray(t) =>
        ujson.Obj("type" -> c.serialize(), "items" -> t.serialize())
      case t => ujson.Str(t.serialize())
    }

}
