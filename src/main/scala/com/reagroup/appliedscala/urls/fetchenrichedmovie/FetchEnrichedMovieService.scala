package com.reagroup.appliedscala.urls.fetchenrichedmovie

import cats.effect.IO
import com.reagroup.appliedscala.models._
import cats.implicits._

class FetchEnrichedMovieService(fetchMovie: MovieId => IO[Option[Movie]],
                                fetchMetascore: MovieName => IO[Option[Metascore]]) {

  /**
    * In order to construct an `EnrichedMovie`, we need to first get a `Movie` and a `Metascore`.
    * We can do so using the functions that are passed in as dependencies.
    *
    * For the purpose of this exercise, let's raise an `EnrichmentFailure` if the `Metascore` does not exist.
    *
    * Hint: We know we are going to be chaining multiple effects in `IO` so let's start a for-comprehension.
    * Also pattern match on `Option` if you're stuck!
    * */
  def fetch(movieId: MovieId): IO[Option[EnrichedMovie]] = for {
    maybeMovie <- fetchMovie(movieId)
    enrich <- maybeMovie match {
      case None => IO.pure(None)
      case Some(movie) => enrichMovieWithMetascore(movie).map(x => Some(x))
    }
  } yield enrich


  //    fetchMovie(movieId).flatMap {
  //    case None => IO.pure(None)
  //    case Some(movie) => enrichMovieWithMetascore(movie).map(em => Some(em))
  //  }

  /**
    * Given a `Movie`, we can call `fetchMetascore` using the `name` of the `Movie`.
    * If no `Metascore` is found, raise an `EnrichmentFailure` using `IO.raiseError`.
    * */
  private def enrichMovieWithMetascore(movie: Movie): IO[EnrichedMovie] = fetchMetascore(movie.name).flatMap {
    case None => IO.raiseError(EnrichmentFailure(movie.name))
    case Some(meta) => IO.pure(EnrichedMovie(movie, meta))
  }

  //    for {
  //    maybeMeta <- fetchMetascore(movie.name)
  //    em <- maybeMeta match {
  //      case None => IO.raiseError(EnrichmentFailure(movie.name))
  //      case Some(meta) => IO.pure(EnrichedMovie(movie, meta))
  //    }
  //  } yield em
}
