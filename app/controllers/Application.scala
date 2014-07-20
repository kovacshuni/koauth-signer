package controllers

import play.api.mvc._

import scala.concurrent.Future

object Application extends Controller {

  def requestToken = Action.async { request =>
    Future {
      Ok(views.html.requestToken("Your new application is ready."))
    }
  }
}
