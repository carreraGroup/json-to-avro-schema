package io.carrera.jsontoavroschema

import io.lemonlabs.uri.Uri
import io.lemonlabs.uri.typesafe.dsl._

object SymbolResolver {
  def resolve(schema: JsonSchema): Map[Uri, Uri] =
    definitions(schema.definitions)

  private def definitions(defs: Map[String, JsonSchema]) = {
    val resolved =
      defs.flatMap { case (name, definition) =>
        val canonical = Uri.parse(s"#/definitions/$name")
        definition.id.map(id => (canonical, id))
      }

    val plusFlipped =
      resolved ++ resolved.map{ case (canonical, id) =>
        (id, canonical)
      }

    plusFlipped ++ defs.flatMap { case (name, schema) =>
      resolve(schema)
    }
  }

}
