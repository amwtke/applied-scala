package com.reagroup.appliedscala.urls.savereview

import cats.Applicative.ops.toAllApplicativeOps
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.effect.IO
import cats.implicits.catsSyntaxValidatedId
import com.reagroup.appliedscala.models.{Movie, MovieId}

class SaveReviewService(saveReview: (MovieId, ValidatedReview) => IO[ReviewId],
                        fetchMovie: MovieId => IO[Option[Movie]]) {

  /**
    * Before saving a `NewReviewRequest`, we want to check that the movie exists and then
    * validate the request in order to get a `ValidatedReview`.
    * Complete `NewReviewValidator`, then use it here before calling `saveReview`.
    * Return all errors encountered whether the movie exists or not.
    *
    */
  def save(movieId: MovieId, review: NewReviewRequest): IO[ValidatedNel[ReviewValidationError, ReviewId]] =
  //    for {
  //      movie <- fetchMovie(movieId)
  //      reviewId <- validateMovie(movie)
  //        .productR(validateReview(review))
  //        .traverse(r => saveReview(movieId, r))
  //    } yield reviewId
  {
    fetchMovie(movieId)
      .map(validateMovie)
      .map(x => x.productR(validateReview(review)))
      .flatMap {
        case Invalid(e) => IO.pure(e.invalid)
        case Valid(review) => saveReview(movieId, review).map(x => x.valid)
      }
  }

  private def validateMovie(maybeMovie: Option[Movie]): ValidatedNel[ReviewValidationError, Movie] =
    maybeMovie match {
      case None => Invalid(MovieDoesNotExist).toValidatedNel
      case Some(value) => Valid(value)
    }

  private def validateReview(review: NewReviewRequest): ValidatedNel[ReviewValidationError, ValidatedReview] = NewReviewValidator.validate(review)

}
