package com.reagroup.appliedscala.urls.fetchenrichedmovie

import com.reagroup.appliedscala.models.Movie
import io.circe.syntax._
import io.circe.{Encoder, Json}

case class EnrichedMovie(movie: Movie, metascore: Metascore)

object EnrichedMovie {

  /**
    * Add an Encoder instance here
    *
    * We want the Json to look like:
    *
    * {
    * "name": "Batman",
    * "synopsis": "Great movie for the family",
    * "reviews": []
    * "metascore": 75
    * }
    *
    * not:
    *
    * {
    * "movie": {
    * "name": "Batman",
    * "synopsis": "Great movie for the family",
    * "reviews": []
    * },
    * "metascore": 75
    * }
    *
    * which is what we would get if we used `deriveEncoder[EnrichedMovie]`
    *
    * Hint: You will need to create a custom encoder (see how we did it for `MovieId`).
    */
  implicit val encoder: Encoder[EnrichedMovie] = {
    case EnrichedMovie(Movie(name, synopsis, reviews), Metascore(value)) =>
      Json.obj(
        "name" -> name.asJson,
        "synopsis" -> synopsis.asJson,
        "reviews" -> reviews.asJson,
        "metascore" -> value.asJson
      )
  }
}
