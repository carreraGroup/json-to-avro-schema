package io.carrera.jsontoavroschema

object AvroWriter {

  def toJson(record: AvroRecord): Either[TranspileError, ujson.Obj] = {
    val result =
      ujson.Obj(
        "type" -> "record",
        "name" -> record.name,
        "fields" -> record.fields.map(toJson)
      )
    record.doc.map(d => result("doc") = d)

    Right(result)
  }

  private def toJson(field: AvroField): ujson.Obj = {
    val result =
      ujson.Obj(
        "name" -> field.name,
        "type" -> serializeType(field.`type`)
      )
    field.doc.map(d => result("doc") = d)

    result
  }

  private def serializeType(avroType: AvroType) =
    avroType match {
      case _: AvroString => "string"
    }

}
