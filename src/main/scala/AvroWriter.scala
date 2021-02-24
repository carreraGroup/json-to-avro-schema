package io.carrera.jsontoavroschema

object AvroWriter {

  def toJson(record: AvroRecord): Either[TranspileError, ujson.Obj] = {
    val result = {
      ujson.Obj(
        "type" -> "record",
        "name" -> record.name,
        "fields" -> record.fields.map(toJson)
      )
    }

    record.namespace.foreach(ns => result("namespace") = ns)
    record.doc.foreach(d => result("doc") = d)

    Right(result)
  }

  private def toJson(field: AvroField): ujson.Obj = {
    val result =
      ujson.Obj(
        "name" -> field.name,
        "type" -> AvroType.serialize(field.`type`)
      )
    field.doc.foreach(d => result("doc") = d)

    result
  }

}
