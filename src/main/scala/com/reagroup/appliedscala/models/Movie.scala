package com.reagroup.appliedscala.models

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

case class Movie(name: String, synopsis: String, reviews: Vector[Review])

object Movie {

  /**
    * Add an Encoder instance here
    *
    * Hint: Use `deriveEncoder`
    */
  implicit val encoder: Encoder.AsObject[Movie] = deriveEncoder[Movie]
  implicit val decoder: Decoder[Movie] = deriveDecoder[Movie]
}
