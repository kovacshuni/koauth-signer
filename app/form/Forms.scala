package form

import com.hunorkovacs.koauth.domain.KoauthRequest
import com.hunorkovacs.koauth.domain.OauthParams._
import play.api.data.Form
import play.api.data.Forms._

object Forms {

  val PostParamName1: String = "postParamName1"
  val PostParamVal1: String = "postParamVal1"
  val PostParamName2: String = "postParamName2"
  val PostParamVal2: String = "postParamVal2"

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
      "method" -> optional(text),
      "url" -> optional(text),
      ConsumerKeyName -> optional(text),
      ConsumerSecretName -> optional(text),
      TokenName -> optional(text),
      TokenSecretName -> optional(text),
      SignatureMethodName -> optional(text),
      TimestampName -> optional(text),
      NonceName -> optional(text),
      VersionName -> optional(text),
      CallbackName -> optional(text),
      VerifierName -> optional(text),
      PostParamName1 -> optional(text),
      PostParamVal1 -> optional(text),
      PostParamName2 -> optional(text),
      PostParamVal2 -> optional(text)
    )(fromGeneralForm)(toGeneralForm)
  )

  private def fromGeneralForm(method: Option[String], url: Option[String], consumerKey: Option[String], consumerSecret: Option[String],
                              tokenName: Option[String], tokenSecret: Option[String],
                              signatureMethod: Option[String], timestamp: Option[String],
                              nonce: Option[String], version: Option[String], callback: Option[String],
                              verifier: Option[String], postParamName1: Option[String],
                              postParamVal1: Option[String], postParamName2: Option[String],
                              postParamVal2: Option[String]): KoauthRequest = {
    def appendIfExists(key: Option[String], value: Option[String], list: List[(String, String)]): List[(String, String)] = {
      value match {
        case None => list
        case Some(someValue) =>
          key match {
            case None => list.::(("", someValue))
            case Some(someKey) => list.::((someKey, someValue))
          }
      }
    }

    var list = List.empty[(String, String)]
    list = appendIfExists(Some(ConsumerKeyName), consumerKey, list)
    list = appendIfExists(Some(ConsumerSecretName), consumerSecret, list)
    list = appendIfExists(Some(TokenName), tokenName, list)
    list = appendIfExists(Some(TokenSecretName), tokenSecret, list)
    list = appendIfExists(Some(SignatureMethodName), signatureMethod, list)
    list = appendIfExists(Some(TimestampName), timestamp, list)
    list = appendIfExists(Some(NonceName), nonce, list)
    list = appendIfExists(Some(VersionName), version, list)
    list = appendIfExists(Some(CallbackName), callback, list)
    list = appendIfExists(Some(VerifierName), verifier, list)

    var bodyParamsList = List.empty[(String, String)]
    bodyParamsList = appendIfExists(postParamName1, postParamVal1, bodyParamsList)
    bodyParamsList = appendIfExists(postParamName2, postParamVal2, bodyParamsList)

    KoauthRequest(method.getOrElse(""), url.getOrElse(""), List.empty, bodyParamsList, list)
  }

  private def toGeneralForm(request: KoauthRequest): Option[(Option[String], Option[String], Option[String], Option[String], Option[String], Option[String],
    Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String], Option[String])] = {

    def getOrElseBodyParams(i: Int) =
      if (request.bodyParams.isDefinedAt(i)) (Some(request.bodyParams(i)._1), Some(request.bodyParams(i)._2))
      else (None, None)

    Some((Some(request.method),
      Some(request.urlWithoutParams),
      request.oauthParamsMap.get(ConsumerKeyName),
      request.oauthParamsMap.get(ConsumerSecretName),
      request.oauthParamsMap.get(TokenName),
      request.oauthParamsMap.get(TokenSecretName),
      request.oauthParamsMap.get(SignatureMethodName),
      request.oauthParamsMap.get(TimestampName),
      request.oauthParamsMap.get(NonceName),
      request.oauthParamsMap.get(VersionName),
      request.oauthParamsMap.get(CallbackName),
      request.oauthParamsMap.get(VerifierName),
      getOrElseBodyParams(0)._1,
      getOrElseBodyParams(0)._2,
      getOrElseBodyParams(1)._1,
      getOrElseBodyParams(1)._2)
    )
  }
}
