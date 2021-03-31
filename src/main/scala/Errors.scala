package io.carrera.jsontoavroschema

final case class ParserError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class ResolutionError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class TranspileError(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)