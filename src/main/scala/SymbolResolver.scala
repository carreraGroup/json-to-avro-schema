package io.carrera.jsontoavroschema

import io.lemonlabs.uri.{Uri, Url}
import io.lemonlabs.uri.typesafe.dsl._

object SymbolResolver {
  type Symbols = Map[Uri,Uri]
  def resolve(schema: JsonSchema): Symbols =
    resolve(schema, Url.parse("#"))

  private def resolve(schema: JsonSchema, ctx: Url): Symbols = {
    def newCtx(path: String) =
      Url.parse(s"#${ctx.fragment.get}/$path")

    resolveSchemas(schema.definitions, newCtx("definitions")) ++
      resolveSchemas(schema.properties, newCtx("properties"))
  }

  private def resolveSchemas(schemas: Map[String, JsonSchema], ctx: Url) = {
    val resolved =
      schemas.flatMap { case (name, schema) =>
        val canonical = Uri.parse(s"$ctx/$name")
        schema.id.map(id => (canonical, id))
      }

    val plusFlipped =
      resolved ++ resolved.map{ case (canonical, id) =>
        (id, canonical)
      }

    plusFlipped ++ schemas.flatMap { case (name, schema) =>
      resolve(schema, Url.parse(s"$ctx/$name"))
    }
  }
}
