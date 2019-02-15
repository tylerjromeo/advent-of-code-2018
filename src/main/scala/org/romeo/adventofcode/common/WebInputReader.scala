package org.romeo.adventofcode.common

import scalaj.http.Http

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 8:39 PM
  *
  * simple class that calls a url with http get and returns the result as a string.
  * takes in an optional auth cookie to pass as a header
  */
class WebInputReader(url: String, authCookie: Option[String] = None) {
  val content: Either[String, String] = {
    val response = authCookie match {
      case Some(c) => Http(url).header("Cookie", c).asString
      case None => Http(url).asString
    }
    if (response.isError) {
      Left(response.code + ": " + response.body)
    } else {
      Right(response.body.trim)
    }
  }
}
