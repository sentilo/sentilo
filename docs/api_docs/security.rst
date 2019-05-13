Security
========

Securing API requests
---------------------

The platform will validate any request received by the system following
the terminology AAA (*Authentication, Authorization, Accounting*):

-  **Authentication**: Identifying who is doing the request.
-  **Authorization**: Validating that the action requested on the
   resource associated can be done.
-  **Traceability**: Auditing the action and who has performed it.

So, for each request received, the platform performs the following
actions:

-  Identify the petitioner through the header HTTP.
-  Check that it can do the requested action on the resource indicated.
-  Register the performed action.

When necessary, the platform will check the integrity and
confidentiality of communications is ensured using **HTTPS** protocol.

Authentication
~~~~~~~~~~~~~~

To identify the petitioner, the platform uses an authentication
mechanism based on tokens (**Token Based Authentication**).

It’s necessary to establish a distribution mechanism outside the
platform for send the tokens among the different users of the platforms
securely. Future versions of Sentilo will include this feature.

The token will be included in the request by adding a header with key
**IDENTITY_KEY.**

An example of a service request (GET in this case) using the curl tool:

::

   curl --request GET --header "IDENTITY_KEY: <YOUR_KEY>" http://<your_api_server.com>/resource

In case of incorrect or invalid token , the platform will respond with
an error code 401.

Authorization
~~~~~~~~~~~~~

To validate the requested action on the resource indicated in the
request can be performed, the platform uses a permit system that checks
authorized entity ( provider or application ) is allowed to admin, write
or read in a resource.

These permissions are defined via the catalog console of the platform
and, by default, every entity is administrable by its owner.

If an action on a resource is done without the appropiate permission,
platform will return an error 403.

Securing Callbacks
-------------------

If it’s necessary to secure the push requests sent by the platform,
Sentilo provides a
`HMAC <http://en.wikipedia.org/wiki/Hash-based_message_authentication_code>`__
mechanism for the callbacks.

This mechanisms guarantees:

-  That the message was sent by the platform
-  That the message was not altered after sent
-  That the messege is still active

As hash algorithm the system uses
`SHA-512 <http://en.wikipedia.org/wiki/SHA-2>`__. It accepts keys of any
size, and produces a hash sequence of length 512 bits.

The target system should activate the security for callbacks when
creates the subscription specifying the secret key (`see
more <./services/subscription/subscription.html>`__). This subscription
**should be done using HTTPs protocol** to avoid compromising the key.

After the subscription has been created, all the related requests will
include two new headers, one with the hash (**Sentilo-Content-Hmac**)
and another with the timestamp (**Sentilo-Date**), as the following
sample shows:

::

   Sentilo-Content-Hmac: 
   j1OQ+fU667GQoHYHWzLBpigRjLJmRvYn53KHZhApTbrcphYWBlRPSBHkntODuqsqx11Vj8rsc7DDziiutTq/5g==
   Sentilo-Date: 10/06/2014T15:27:22

The responsibility of validating the headers will be always in the
target system who is receiving the messages.

The pseudo-code to generate the HMAC token is the following:

::

   var md5Body = MD5(body)
   var endpoint = endpoint_configured_in_subscription
   var secretKey = secret_key_configured_in_subscription
   var currentDate = value_http_header_Sentilo-Date
   var contentToSign = concatenate('POST',md5Body, 'application/json',currentDate, endpoint)
   var signature = HmacSHA512(contentToSign)

   return base64UrlEncode(signature)
