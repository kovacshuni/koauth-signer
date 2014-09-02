# KOAuth Signer

Application helping to sign HTTP requests according to the [OAuth 1.0a](http://oauth.net/core/1.0a/)
specifications.

## How to run it:

1. Clone this repository: `git clone https://github.com/kovacshuni/koauth-signer.git`
2. Run with sbt: `sbt run`
3. Open in browser: [http://localhost:9000](http://localhost:9000)
  * [Request Token Request](http://localhost:9000/request-token)
  * [Authorize Request Token Request](http://localhost:9000/authorize)
  * [Acquire Access Token Request](http://localhost:9000/access-token)
  * [Sign basic request. Accessing protected resources](http://localhost:9000/oauthenticate)
  * [Sign general request (without any restrictions)](http://localhost:9000/general)

## Owner

Hunor Kovács  
kovacshuni@yahoo.com  
[hunorkovacs.com](http://www.hunorkovacs.com)

## Licence

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) .
