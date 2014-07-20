package controllers

import play.api.mvc._

import scala.concurrent.Future
import com.hunorkovacs.koauth.service.consumer.{DefaultConsumerService, ConsumerService}

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def requestToken = Action.async { request =>
    DefaultConsumerService.createRequestTokenRequest()

    Future {
      val l = List(("oauth_nonce", "123456"))
      Ok(views.html.requestToken(l.toMap))
    }
  }
}
