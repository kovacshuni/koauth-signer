package controllers

import com.hunorkovacs.koauth.domain.OauthParams._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.concurrent.Future
import com.hunorkovacs.koauth.service.consumer.{DefaultConsumerService, ConsumerService}

object Application extends Controller {

  import scala.concurrent.ExecutionContext.Implicits.global

  val requestTokenForm = Form(
    mapping(
      "method" -> text,
      "url" -> text,
      consumerKeyName -> text,
      consumerSecretName -> text,
      timestampName -> text,
      nonceName -> text,
      signatureName -> text,
      signatureMethodName -> text,
      versionName -> text
    )(
      (method,
       url,
       consumerKey,
       consumerSecret,
       timestamp,
       nonce,
       signature,
       signatureMethod,
       version,
       _) =>
        val l = List((consumerKeyName, consumerKey),
          (consumerSecretName, consumerSecret))
        val m = l.toMap
        com.hunorkovacs.koauth.domain.Request(method, url, List.empty, List.empty, l, m)
      (r: com.hunorkovacs.koauth.domain.Request) =>
        Some(r.method, r.urlWithoutParams, r.urlParams, r.bodyParams, r.oauthParamsList, r.oauthParamsMap)
    )

//  val filledForm = loginForm.fill(User("Bob", 18))

  def requestToken = Action.async { request =>
//    DefaultConsumerService.createRequestTokenRequest()

    Future {
      val l = List(("oauth_nonce", "123456"))
      Ok(views.html.requestToken(l.toMap))
    }
  }
}
