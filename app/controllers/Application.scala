package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future
import com.hunorkovacs.koauth.service.consumer.{DefaultConsumerService, ConsumerService}
import com.hunorkovacs.koauth.domain.Request

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

//  val filledForm = loginForm.fill(User("Bob", 18))

  def requestToken = Action.async { request =>
    Future {
      val formData = requestTokenForm.bindFromRequest().get
      
      val l = List(("oauth_nonce", "123456"))
      Ok(views.html.requestToken(l.toMap))
    }
  }

  val requestTokenForm = Form[Request](
    mapping(
      "method" -> text,
      "url" -> text,
      consumerKeyName -> text,
      consumerSecretName -> text
    )(from)(to)
  )

  private def from(method: String, url: String, consumerKey: String, consumerSecret: String): Request = {
    val list = List((consumerKeyName, consumerKey),
      (consumerSecretName, consumerSecret))
    Request("", "", List.empty, List.empty, list, list.toMap)
  }

  private def to(request: Request): Option[(String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(consumerKeyName),
      request.oauthParamsMap(consumerSecretName))
  }
}
