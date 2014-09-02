package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.{DefaultConsumerService, RequestWithInfo}
import form.Forms._
import play.api.data.Form
import play.api.mvc._
import play.twirl.api.HtmlFormat.Appendable

import scala.concurrent.Future
import com.hunorkovacs.koauth.domain.KoauthRequest

object Application extends Controller {

  private val Post = "POST"

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  private val consumer = new DefaultConsumerService(ec)
  import consumer._

  def index = Action.async(_ => Future(Ok(views.html.index())))

  def requestTokenGet =
    get(emptyEmptyRequesToken, requestTokenForm, views.html.requestToken.apply, StrictlyPost())

  def requestTokenPost =
    post(createRequestToken, requestTokenForm, views.html.requestToken.apply, StrictlyPost())

  private def emptyEmptyRequesToken(emptyRequest: KoauthRequest) =
    createRequestTokenRequest(emptyRequest, "", "", "")

  private def createRequestToken(request: KoauthRequest) = {
    val consumerKey = request.oauthParamsMap(ConsumerKeyName)
    val consumerSecret = request.oauthParamsMap(ConsumerSecretName)
    val callback = request.oauthParamsMap(CallbackName)
    createRequestTokenRequest(request, consumerKey, consumerSecret, callback)
  }

  def authorizeGet =
    get(createEmptyAuthorize, authorizeForm, views.html.authorize.apply, StrictlyPost())

  def authorizePost =
    post(createAuthorize, authorizeForm, views.html.authorize.apply, StrictlyPost())

  private def createEmptyAuthorize(emptyRequest: KoauthRequest) =
    createAuthorizeRequest(emptyRequest, "", "", "", "", "", "")

  private def createAuthorize(request: KoauthRequest) = {
    val consumerKey = request.oauthParamsMap(ConsumerKeyName)
    val consumerSecret = request.oauthParamsMap(ConsumerSecretName)
    val token = request.oauthParamsMap(TokenName)
    val tokenSecret = request.oauthParamsMap(TokenSecretName)
    val username = request.oauthParamsMap(UsernameName)
    val password = request.oauthParamsMap(PasswordName)
    createAuthorizeRequest(request, consumerKey, consumerSecret, token, tokenSecret, username, password)
  }

  def accessTokenGet =
    get(createEmptyAccessToken, accessTokenForm, views.html.accessToken.apply, StrictlyPost())

  def accessTokenPost =
    post(createAccessToken, accessTokenForm, views.html.accessToken.apply, StrictlyPost())

  private def createEmptyAccessToken(emptyRequest: KoauthRequest) =
    createAccessTokenRequest(emptyRequest, "", "", "", "", "")

  private def createAccessToken(request: KoauthRequest) = {
    val consumerKey = request.oauthParamsMap(ConsumerKeyName)
    val consumerSecret = request.oauthParamsMap(ConsumerSecretName)
    val token = request.oauthParamsMap(TokenName)
    val tokenSecret = request.oauthParamsMap(TokenSecretName)
    val verifier = request.oauthParamsMap(VerifierName)
    createAccessTokenRequest(request, consumerKey, consumerSecret, token, tokenSecret, verifier)
  }

  def oauthenticateGet =
    get(createEmptyOauthenticate, oauthenticateForm, views.html.oauthenticate.apply, WhateverComes())

  def oauthenticatePost =
    post(createOauthenticate, oauthenticateForm, views.html.oauthenticate.apply, WhateverComes())

  private def createEmptyOauthenticate(emptyRequest: KoauthRequest) =
    createOauthenticatedRequest(emptyRequest, "", "", "", "")

  private def createOauthenticate(request: KoauthRequest) = {
    val consumerKey = request.oauthParamsMap(ConsumerKeyName)
    val consumerSecret = request.oauthParamsMap(ConsumerSecretName)
    val token = request.oauthParamsMap(TokenName)
    val tokenSecret = request.oauthParamsMap(TokenSecretName)
    createOauthenticatedRequest(request, consumerKey, consumerSecret, token, tokenSecret)
  }

  def generalRequestGet = Action.async { request =>
    Future(Ok(views.html.generalRequest(generalRequestForm)("", "")))
  }

  def generalRequestPost =
    post(createGeneralSignedRequest, generalRequestForm, views.html.generalRequest.apply, WhateverComes())

  private type T = Form[KoauthRequest] => (String, String) => Appendable

  private def get(createEmptyRequest: KoauthRequest => Future[RequestWithInfo],
          form: Form[KoauthRequest],
          view: T,
          method: MethodToFill) = Action.async { request =>
    Future(KoauthRequest(method.get, "", List.empty, List.empty, List.empty))
      .flatMap(createEmptyRequest)
      .map { requestAndInfo =>
      val filledForm = form.fill(requestAndInfo.request)
      Ok(view(filledForm)("", ""))
    }
  }

  private def post(createRequest: KoauthRequest => Future[RequestWithInfo],
                   form: Form[KoauthRequest],
                   view: T,
                   method: MethodToFill) = {
    Action.async { implicit request =>
      Future(filterRequest(form.bindFromRequest.get, method))
        .flatMap(createRequest)
        .map { requestAndInfo =>
        val completedForm = form.fill(requestAndInfo.request)
        Ok(view(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
      }
    }
  }

  private def filterRequest(request: KoauthRequest, method: MethodToFill) = {
    val newMap = request.oauthParamsMap.filterNot(p => SignatureMethodName == p._1 ||
      TimestampName == p._1 ||
      NonceName == p._1 ||
      VersionName == p._1)
    KoauthRequest(method.get(request), request.urlWithoutParams, List.empty, List.empty, newMap.toList)
  }

  private trait MethodToFill {
    def get(request: KoauthRequest) = {
      this match {
        case StrictlyPost() => Post
        case WhateverComes() => request.method
      }
    }

    def get() = {
      this match {
        case StrictlyPost() => Post
        case WhateverComes() => ""
      }
    }
  }
  private case class StrictlyPost() extends MethodToFill
  private case class WhateverComes() extends MethodToFill
}
