package io.carrera.jsontoavroschema

object NameSanitizer {
  def sanitize(rootName: String)(record: AvroRecord): AvroRecord = {
    val sanitizer = sanitizeName(rootName) _
    sanitizeType(record, sanitizer).asInstanceOf[AvroRecord]
  }

  private def sanitizeField(field: AvroField, sanitizer: String => String): AvroField = {
    field.copy(`type` = sanitizeType(field.`type`, sanitizer))
  }

  private def sanitizeType(avroType: AvroType, sanitizer: String => String): AvroType = {
    avroType match {
      case AvroRef(name) => AvroRef(sanitizer(name))
      case AvroUnion(types) => AvroUnion(types.map(t => sanitizeType(t, sanitizer)))
      case AvroArray(t) => AvroArray(sanitizeType(t, sanitizer))
      case AvroMap(t) => AvroMap(sanitizeType(t, sanitizer))
      case AvroRecord(name, ns, doc, fields) =>
        AvroRecord(sanitizer(name), ns, doc, fields.map(fld => sanitizeField(fld, sanitizer)))
      case t => t
    }
  }

  private def sanitizeName(rootName: String)(name: String):String =
    name match {
      case "boolean" => s"${rootName}Boolean"
      case "string" => s"${rootName}String"
      case "int" => s"${rootName}Int"
      case "long" => s"${rootName}Long"
      case "float" => s"${rootName}Float"
      case "double" => s"${rootName}Double"
      case "bytes" => s"${rootName}Bytes"
      case "null" => s"${rootName}Null"
      case x => x
    }
}
