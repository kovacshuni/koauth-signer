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

  def requestToken = Action.async { request =>
    Future {
      val emptyRequest = Request("", "", List.empty, List.empty, List.empty, Map.empty)
      Ok(views.html.requestToken(emptyRequest)(requestTokenForm))
    }
  }

  def requestTokenPost = Action.async { request =>
    Future {
      val requestTokenRequest = requestTokenForm.bindFromRequest()(request).get
      val filledForm = requestTokenForm.fill(requestTokenRequest)
      Ok(views.html.requestToken(requestTokenRequest)(filledForm))
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
    Request(method, url, List.empty, List.empty, list, list.toMap)
  }

  private def to(request: Request): Option[(String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(consumerKeyName),
      request.oauthParamsMap(consumerSecretName))
  }
}
