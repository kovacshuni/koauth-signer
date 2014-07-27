package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import com.hunorkovacs.koauth.service.consumer.DefaultConsumerService._
import com.hunorkovacs.koauth.service.consumer.RequestWithInfo
import form.Forms._
import play.api.data.Form
import play.api.mvc._
import play.twirl.api.HtmlFormat.Appendable

import scala.concurrent.Future
import com.hunorkovacs.koauth.domain.KoauthRequest

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  val Method = "POST"

  def requestTokenGet =
    get(emptyEmptyRequesToken, requestTokenForm, views.html.requestToken.apply)

  def requestTokenPost =
    post(createRequestToken, requestTokenForm, views.html.requestToken.apply)

  private def emptyEmptyRequesToken(emptyRequest: KoauthRequest) =
    createRequestTokenRequest(emptyRequest, "", "", "")

  private def createRequestToken(request: KoauthRequest) = {
    val consumerKey = request.oauthParamsMap(ConsumerKeyName)
    val consumerSecret = request.oauthParamsMap(ConsumerSecretName)
    val callback = request.oauthParamsMap(CallbackName)
    createRequestTokenRequest(request, consumerKey, consumerSecret, callback)
  }

  def authorizeGet =
    get(createEmptyAuthorize, authorizeForm, views.html.authorize.apply)

  def authorizePost =
    post(createAuthorize, authorizeForm, views.html.authorize.apply)

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
    get(createEmptyAccessToken, accessTokenForm, views.html.accessToken.apply)

  def accessTokenPost =
    post(createAccessToken, accessTokenForm, views.html.accessToken.apply)

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
    get(createEmptyOauthenticate, oauthenticateForm, views.html.oauthenticate.apply)

  def oauthenticatePost =
    post(createOauthenticate, oauthenticateForm, views.html.oauthenticate.apply)

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
    post(createGeneralSignedRequest, generalRequestForm, views.html.generalRequest.apply)

  private type T = Form[KoauthRequest] => (String, String) => Appendable

  private def get(createEmptyRequest: KoauthRequest => Future[RequestWithInfo],
          form: Form[KoauthRequest],
          view: T) = Action.async { request =>
    Future(KoauthRequest(Method, "", List.empty, List.empty, List.empty))
      .flatMap(createEmptyRequest)
      .map { requestAndInfo =>
      val filledForm = form.fill(requestAndInfo.request)
      Ok(view(filledForm)("", ""))
    }
  }

  private def post(createRequest: KoauthRequest => Future[RequestWithInfo],
                   form: Form[KoauthRequest],
                   view: T) = {
    Action.async { implicit request =>
      Future(filterRequest(form.bindFromRequest.get))
        .flatMap(createRequest)
        .map { requestAndInfo =>
        val completedForm = form.fill(requestAndInfo.request)
        Ok(view(completedForm)(requestAndInfo.signatureBase, requestAndInfo.header))
      }
    }
  }

  private def filterRequest(request: KoauthRequest) = {
    val newMap = request.oauthParamsMap.filterNot(p => SignatureMethodName == p._1 ||
      TimestampName == p._1 ||
      NonceName == p._1 ||
      VersionName == p._1)
    KoauthRequest(Method, request.urlWithoutParams, List.empty, List.empty, newMap.toList)
  }
}
