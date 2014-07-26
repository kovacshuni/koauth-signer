package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService.{createAuthorizeRequest, createRequestTokenRequest}
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
      val consumerKey = filteredRequest.oauthParamsMap(ConsumerKeyName)
      val consumerSecret = filteredRequest.oauthParamsMap(ConsumerSecretName)
      val callback = filteredRequest.oauthParamsMap(CallbackName)
      createRequestTokenRequest(filteredRequest, consumerKey, consumerSecret, callback)
    } map { requestAndInfo =>
      val completedForm = requestTokenForm.fill(requestAndInfo.request)
      Ok(views.html.requestToken(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
    }
  }

  def authorize = Action.async { request =>
    Future(Ok(views.html.authorize(authorizeForm)("", "")))
  }

  def authorizePost = Action.async { implicit request =>
    Future(filterRequest(authorizeForm.bindFromRequest.get)) flatMap { filteredRequest =>
      val consumerKey = filteredRequest.oauthParamsMap(ConsumerKeyName)
      val consumerSecret = filteredRequest.oauthParamsMap(ConsumerSecretName)
      val token = filteredRequest.oauthParamsMap(TokenName)
      val tokenSecret = filteredRequest.oauthParamsMap(TokenSecretName)
      val username = filteredRequest.oauthParamsMap(UsernameName)
      val password = filteredRequest.oauthParamsMap(PasswordName)
      createAuthorizeRequest(filteredRequest, consumerKey, consumerSecret, token, tokenSecret, username, password)
    } map { requestAndInfo =>
      val completedForm = authorizeForm.fill(requestAndInfo.request)
      Ok(views.html.authorize(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
    }
  }

  private def filterRequest(request: KoauthRequest) = {
    val newMap = request.oauthParamsMap.filterNot(p => SignatureMethodName == p._1 ||
      TimestampName == p._1 ||
      NonceName == p._1 ||
      VersionName == p._1)
    KoauthRequest(Method, request.urlWithoutParams, List.empty, List.empty, newMap.toList)
  }

  val requestTokenForm = Form[KoauthRequest](
    mapping(
      "method" -> text,
      "url" -> text,
      ConsumerKeyName -> text,
      ConsumerSecretName -> text,
      SignatureMethodName -> text,
      TimestampName -> text,
      NonceName -> text,
      VersionName -> text,
      CallbackName -> text
    )(fromRequestTokenForm)(toRequestTokenForm)
  )

  private def fromRequestTokenForm(method: String, url: String, consumerKey: String, consumerSecret: String, signatureMethod: String,
                   timestamp: String, nonce: String, version: String, callback: String): KoauthRequest = {
    val list = List((ConsumerKeyName, consumerKey),
      (ConsumerSecretName, consumerSecret),
      (SignatureMethodName, signatureMethod),
      (TimestampName, timestamp),
      (NonceName, nonce),
      (VersionName, version),
      (CallbackName, callback))
    KoauthRequest(method, url, List.empty, List.empty, list)
  }

  private def toRequestTokenForm(request: KoauthRequest): Option[(String, String, String, String, String, String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(ConsumerKeyName),
      request.oauthParamsMap(ConsumerSecretName),
      request.oauthParamsMap(SignatureMethodName),
      request.oauthParamsMap(TimestampName),
      request.oauthParamsMap(NonceName),
      request.oauthParamsMap(VersionName),
      request.oauthParamsMap(CallbackName))
  }

  val authorizeForm = Form[KoauthRequest](
    mapping(
      "method" -> text,
      "url" -> text,
      ConsumerKeyName -> text,
      ConsumerSecretName -> text,
      TokenName -> text,
      TokenSecretName -> text,
      SignatureMethodName -> text,
      TimestampName -> text,
      NonceName -> text,
      VersionName -> text,
      UsernameName -> text,
      PasswordName -> text
    )(fromAuthorizeForm)(toAuthorizeForm)
  )

  private def fromAuthorizeForm(method: String, url: String, consumerKey: String, consumerSecret: String,
                                tokenName: String, tokenSecret: String, signatureMethod: String, timestamp: String,
                                nonce: String, version: String, username: String, password: String): KoauthRequest = {
    val list = List((ConsumerKeyName, consumerKey),
      (ConsumerSecretName, consumerSecret),
      (TokenName, tokenName),
      (TokenSecretName, tokenSecret),
      (SignatureMethodName, signatureMethod),
      (TimestampName, timestamp),
      (NonceName, nonce),
      (VersionName, version),
      (UsernameName, username),
      (PasswordName, password))
    KoauthRequest(method, url, List.empty, List.empty, list)
  }

  private def toAuthorizeForm(request: KoauthRequest): Option[(String, String, String, String, String, String,
    String, String, String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(ConsumerKeyName),
      request.oauthParamsMap(ConsumerSecretName),
      request.oauthParamsMap(TokenName),
      request.oauthParamsMap(TokenSecretName),
      request.oauthParamsMap(SignatureMethodName),
      request.oauthParamsMap(TimestampName),
      request.oauthParamsMap(NonceName),
      request.oauthParamsMap(VersionName),
      request.oauthParamsMap(UsernameName),
      request.oauthParamsMap(PasswordName))
  }
}
