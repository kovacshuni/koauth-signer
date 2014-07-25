package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService.createRequestTokenRequest
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future
import com.hunorkovacs.koauth.domain.KoauthRequest

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  val Method = "POST"

  def requestToken = Action.async { request =>
    Future(Ok(views.html.requestToken(requestTokenForm)("", "")))
  }

  def requestTokenPost = Action.async { implicit request =>
    Future(filterRequest(requestTokenForm.bindFromRequest.get)) flatMap { filteredRequest =>
      val consumerKey = filteredRequest.oauthParamsMap(consumerKeyName)
      val consumerSecret = filteredRequest.oauthParamsMap(consumerSecretName)
      val callback = filteredRequest.oauthParamsMap(callbackName)
      createRequestTokenRequest(filteredRequest, consumerKey, consumerSecret, callback)
    } map { requestAndInfo =>
      val completedForm = requestTokenForm.fill(requestAndInfo.request)
      Ok(views.html.requestToken(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
    }
  }

  private def filterRequest(request: KoauthRequest) = {
    val newMap = request.oauthParamsMap.filterNot(p => signatureMethodName == p._1 ||
      timestampName == p._1 ||
      nonceName == p._1 ||
      versionName == p._1)
    new KoauthRequest(Method, request.urlWithoutParams, List.empty, List.empty, newMap.toList)
  }

  val requestTokenForm = Form[KoauthRequest](
    mapping(
      "method" -> text,
      "url" -> text,
      consumerKeyName -> text,
      consumerSecretName -> text,
      signatureMethodName -> text,
      timestampName -> text,
      nonceName -> text,
      versionName -> text,
      callbackName -> text
    )(from)(to)
  )

  private def from(method: String, url: String, consumerKey: String, consumerSecret: String, signatureMethod: String,
                   timestamp: String, nonce: String, version: String, callback: String): KoauthRequest = {
    val list = List((consumerKeyName, consumerKey),
      (consumerSecretName, consumerSecret),
      (signatureMethodName, signatureMethod),
      (timestampName, timestamp),
      (nonceName, nonce),
      (versionName, version),
      (callbackName, callback))
    new KoauthRequest(method, url, List.empty, List.empty, list)
  }

  private def to(request: KoauthRequest): Option[(String, String, String, String, String, String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(consumerKeyName),
      request.oauthParamsMap(consumerSecretName),
      request.oauthParamsMap(signatureMethodName),
      request.oauthParamsMap(timestampName),
      request.oauthParamsMap(nonceName),
      request.oauthParamsMap(versionName),
      request.oauthParamsMap(callbackName))
  }
}
