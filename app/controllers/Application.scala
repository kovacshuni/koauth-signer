package controllers

import java.util.{TimeZone, Calendar}

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService.createRequestTokenRequest
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future
import com.hunorkovacs.koauth.domain.Request

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  val Method = "POST"

  def requestToken = Action.async { request =>
    Future {
      val emptyRequest = Request("", "", List.empty, List.empty, List.empty, Map.empty)
      Ok(views.html.requestToken(requestTokenForm)(""))
    }
  }

  def requestTokenPost = Action.async { implicit request =>
    val complementedRequestF = Future(requestTokenForm.bindFromRequest.get).map(complementRequest)
    val complementedFormF = complementedRequestF.map(requestTokenForm.fill)
    for {
      complementedRequest <- complementedRequestF
      complementedForm <- complementedFormF
      consumerKey = complementedRequest.oauthParamsMap(consumerKeyName)
      consumerSecret = complementedRequest.oauthParamsMap(consumerSecretName)
      callback = complementedRequest.oauthParamsMap(callbackName)
      header <- createRequestTokenRequest(complementedRequest, consumerKey, consumerSecret, callback)
      // but it should give back:
      // Complemented Request
      // SigantureBase
      // Header but that is in Request
      // Signature but that is also in Request, and in header

      // Why is the authorization call done via list instead of request. Make it uniform.
      //
    } yield Ok(views.html.requestToken(complementedForm)(header))
  }

  private def complementRequest(request: Request) = {
    val newMap = request.oauthParamsMap.filterNot(p => signatureMethodName == p._1 ||
      timestampName == p._1 ||
      nonceName == p._1 ||
      versionName == p._1)
    Request(Method, request.urlWithoutParams, List.empty,
      List.empty, newMap.toList, newMap)
  }

  val requestTokenForm = Form[Request](
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
                   timestamp: String, nonce: String, version: String, callback: String): Request = {
    val list = List((consumerKeyName, consumerKey),
      (consumerSecretName, consumerSecret),
      (signatureMethodName, signatureMethod),
      (timestampName, timestamp),
      (nonceName, nonce),
      (versionName, version),
      (callbackName, callback))
    Request(method, url, List.empty, List.empty, list, list.toMap)
  }

  private def to(request: Request): Option[(String, String, String, String, String, String, String, String, String)] = {
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
