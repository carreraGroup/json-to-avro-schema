package io.carrera.jsontoavroschema

import io.lemonlabs.uri.{Uri, Url}
import io.lemonlabs.uri.typesafe.dsl._

object SymbolResolver {
  def resolve(schema: JsonSchema): Map[Uri, Uri] =
    resolve(schema, Url.parse("#"))

  private def resolve(schema: JsonSchema, ctx: Url): Map[Uri,Uri] =
    definitions(schema.definitions, ctx)

  private def definitions(defs: Map[String, JsonSchema], ctx: Url) = {
    val newCtx = s"#${ctx.fragment.get}/definitions"
    val resolved =
      defs.flatMap { case (name, definition) =>
        val canonical = Uri.parse(s"$newCtx/$name")
        definition.id.map(id => (canonical, id))
      }

    val plusFlipped =
      resolved ++ resolved.map{ case (canonical, id) =>
        (id, canonical)
      }

    plusFlipped ++ defs.flatMap { case (name, schema) =>
      resolve(schema, Url.parse(s"$newCtx/$name"))
    }
  }

}
