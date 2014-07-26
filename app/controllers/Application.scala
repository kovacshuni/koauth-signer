package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.{RequestWithInfo, DefaultConsumerService}
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.twirl.api.{Format, HtmlFormat, BaseScalaTemplate}

import scala.concurrent.{ExecutionContext, Future}
import com.hunorkovacs.koauth.domain.KoauthRequest

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  val Method = "POST"

  def requestTokenGet = Action.async { request =>
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

  def authorizeGet = Action.async { request =>
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

  def accessTokenGet = Action.async { request =>
    Future(Ok(views.html.accessToken(accessTokenForm)("", "")))
  }

  def accessTokenPost = Action.async { implicit request =>
    Future(filterRequest(accessTokenForm.bindFromRequest.get)) flatMap { filteredRequest =>
      val consumerKey = filteredRequest.oauthParamsMap(ConsumerKeyName)
      val consumerSecret = filteredRequest.oauthParamsMap(ConsumerSecretName)
      val token = filteredRequest.oauthParamsMap(TokenName)
      val tokenSecret = filteredRequest.oauthParamsMap(TokenSecretName)
      val verifier = filteredRequest.oauthParamsMap(VerifierName)
      createAccessTokenRequest(filteredRequest, consumerKey, consumerSecret, token, tokenSecret, verifier)
    } map { requestAndInfo =>
      val completedForm = accessTokenForm.fill(requestAndInfo.request)
      Ok(views.html.accessToken.apply(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
    }
  }

  def oauthenticateGet =  Action.async { request =>
    Future(Ok(views.html.oauthenticate(oauthenticateForm)("", "")))
  }

  def oauthenticatePost = Action.async { implicit request =>
    Future(filterRequest(oauthenticateForm.bindFromRequest.get)) flatMap { filteredRequest =>
      val consumerKey = filteredRequest.oauthParamsMap(ConsumerKeyName)
      val consumerSecret = filteredRequest.oauthParamsMap(ConsumerSecretName)
      val token = filteredRequest.oauthParamsMap(TokenName)
      val tokenSecret = filteredRequest.oauthParamsMap(TokenSecretName)
      createOauthenticatedRequest(filteredRequest, consumerKey, consumerSecret, token, tokenSecret)
    } map { requestAndInfo =>
      val completedForm = oauthenticateForm.fill(requestAndInfo.request)
      Ok(views.html.oauthenticate.apply(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
    }
  }

  def generalRequestGet = Action.async { request =>
    Future(Ok(views.html.generalRequest(generalRequestForm)("", "")))
  }

  def generalRequestPost = Action.async { implicit request =>
    Future(generalRequestForm.bindFromRequest.get)
      .flatMap(createGeneralSignedRequest)
      .map { requestAndInfo =>
      val completedForm = generalRequestForm.fill(requestAndInfo.request)
      Ok(views.html.oauthenticate.apply(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
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

  val accessTokenForm = Form[KoauthRequest](
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
      VerifierName -> text
    )(fromAccessTokenForm)(toAccessTokenForm)
  )

  private def fromAccessTokenForm(method: String, url: String, consumerKey: String, consumerSecret: String,
                                tokenName: String, tokenSecret: String, signatureMethod: String, timestamp: String,
                                nonce: String, version: String, verifier: String): KoauthRequest = {
    val list = List((ConsumerKeyName, consumerKey),
      (ConsumerSecretName, consumerSecret),
      (TokenName, tokenName),
      (TokenSecretName, tokenSecret),
      (SignatureMethodName, signatureMethod),
      (TimestampName, timestamp),
      (NonceName, nonce),
      (VersionName, version),
      (VerifierName, verifier))
    KoauthRequest(method, url, List.empty, List.empty, list)
  }

  private def toAccessTokenForm(request: KoauthRequest): Option[(String, String, String, String, String, String,
    String, String, String, String, String)] = {
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
      request.oauthParamsMap(VerifierName))
  }

  val oauthenticateForm = Form[KoauthRequest](
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
      VersionName -> text
    )(fromOauthenticateForm)(toOauthenticateForm)
  )

  private def fromOauthenticateForm(method: String, url: String, consumerKey: String, consumerSecret: String,
                                  tokenName: String, tokenSecret: String, signatureMethod: String, timestamp: String,
                                  nonce: String, version: String): KoauthRequest = {
    val list = List((ConsumerKeyName, consumerKey),
      (ConsumerSecretName, consumerSecret),
      (TokenName, tokenName),
      (TokenSecretName, tokenSecret),
      (SignatureMethodName, signatureMethod),
      (TimestampName, timestamp),
      (NonceName, nonce),
      (VersionName, version))
    KoauthRequest(method, url, List.empty, List.empty, list)
  }

  private def toOauthenticateForm(request: KoauthRequest): Option[(String, String, String, String, String, String,
    String, String, String, String)] = {
    Some(request.method,
      request.urlWithoutParams,
      request.oauthParamsMap(ConsumerKeyName),
      request.oauthParamsMap(ConsumerSecretName),
      request.oauthParamsMap(TokenName),
      request.oauthParamsMap(TokenSecretName),
      request.oauthParamsMap(SignatureMethodName),
      request.oauthParamsMap(TimestampName),
      request.oauthParamsMap(NonceName),
      request.oauthParamsMap(VersionName))
  }

  val generalRequestForm = Form[KoauthRequest](
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
      CallbackName -> text,
      VerifierName -> text,
      "postParamName1" -> text,
      "postParamVal1" -> text,
      "postParamName2" -> text,
      "postParamVal2" -> text
    )(fromGeneralForm)(toGeneralForm)
  )

  private def fromGeneralForm(method: String, url: String, consumerKey: String, consumerSecret: String,
                                    tokenName: String, tokenSecret: String,
                                    signatureMethod: String, timestamp: String,
                                    nonce: String, version: String, callback: String,
                                    verifier: String, postParamName1: String,
                                    postParamVal1: String, postParamName2: String,
                                    postParamVal2: String): KoauthRequest = {
    def appendIfExists(key: String, value: String, list: List[(String, String)]): List[(String, String)] = {
      if (value.isEmpty) list
      else list.::((key, value))
    }

    var list = List.empty[(String, String)]
    list = appendIfExists(ConsumerKeyName, consumerKey, list)
    list = appendIfExists(ConsumerSecretName, consumerSecret, list)
    list = appendIfExists(TokenName, tokenName, list)
    list = appendIfExists(TokenSecretName, tokenSecret, list)
    list = appendIfExists(SignatureMethodName, signatureMethod, list)
    list = appendIfExists(TimestampName, timestamp, list)
    list = appendIfExists(NonceName, nonce, list)
    list = appendIfExists(VersionName, version, list)
    list = appendIfExists(CallbackName, callback, list)
    list = appendIfExists(VerifierName, verifier, list)

    var bodyParamsList = List.empty[(String, String)]
    bodyParamsList = appendIfExists(postParamName1, postParamVal1, bodyParamsList)
    bodyParamsList = appendIfExists(postParamName2, postParamVal2, bodyParamsList)

    KoauthRequest(method, url, List.empty, bodyParamsList, list)
  }

  private def toGeneralForm(request: KoauthRequest): Option[(String, String, String, String, String, String,
    String, String, String, String, String, String, String, String, String, String)] = {
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
      request.oauthParamsMap(CallbackName),
      request.oauthParamsMap(VerifierName),
      request.bodyParams.applyOrElse(0, x => ("", ""))._1,
      request.bodyParams.applyOrElse(0, x => ("", ""))._2,
      request.bodyParams.applyOrElse(1, x => ("", ""))._1,
      request.bodyParams.applyOrElse(1, x => ("", ""))._2
    )
  }
}
