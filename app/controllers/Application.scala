package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService.{createSignatureBase, createRequestTokenRequest}
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future
import com.hunorkovacs.koauth.service.consumer.{DefaultConsumerService, ConsumerService}
import com.hunorkovacs.koauth.domain.{OauthParams, Request}

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  def requestToken = Action.async { request =>
    Future {
      val emptyRequest = Request("", "", List.empty, List.empty, List.empty, Map.empty)
      Ok(views.html.requestToken(requestTokenForm)(""))
    }
  }

  def requestTokenPost = Action.async { request =>
    val postedRequestF = Future(requestTokenForm.bindFromRequest()(request).get)
    val filledFormF = postedRequestF.map(requestTokenForm.fill)
    val headerF = postedRequestF flatMap { postedRequest =>
      val consumerKey = postedRequest.oauthParamsMap(consumerKeyName)
      val consumerSecret = postedRequest.oauthParamsMap(consumerSecretName)
      val callback = postedRequest.oauthParamsMap(callbackName)
      createRequestTokenRequest(postedRequest, consumerKey, consumerSecret, callback)
    }
    for {
      filledForm <- filledFormF
      header <- headerF
    } yield Ok(views.html.requestToken(filledForm)(header))
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
