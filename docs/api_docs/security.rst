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

.. note::

   Sentilo itself does not provide a mechanism to SSL/TLS http protocol (inbound https requests).
   That's because we don't need to duplicate the work of others - Sentilo is typically deployed
   behind a reverse proxy or an API manager, which already handle the encrypted connections very well.


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
and, by default, every entity can be administrated by its owner.

If an action on a resource is done without the appropriate permission,
platform will return an error 403.

Securing Callbacks
-------------------

If it's necessary to secure the push requests sent by the platform, Sentilo provides a
`HMAC <http://en.wikipedia.org/wiki/Hash-based_message_authentication_code>`__ mechanism for the callbacks.

This mechanism guarantees that:

-  the message was sent by the platform
-  the message was not altered after being sent
-  it was rend to a specific endpoint of the subscription

How does it work?
~~~~~~~~~~~~~~~~~

The data flow is the following:

0. Prerequisite: A `Subscription <./services/subscription/subscription.html>`__ is created via
   API with a :literal:`secretCallbackKey`. You may subscribe to any event type.
1. The data/alarm/command event is generated. How the event is created and by which entity is irrelevant.
2. If there is a Subscription with a :literal:`secretCallbackKey` for this event, the message will be signed and
   headers :literal:`X-Sentilo-Content-Hmac` and :literal:`X-Sentilo-Date` will be created.
3. The external system receives the subscription and may check its authenticity using a same :literal:`secretCallbackKey`.



The system uses the `SHA-512 <http://en.wikipedia.org/wiki/SHA-2>`__ algorithm.
It accepts keys of any size, and produces a hash sequence of length 512 bits.

The target system should activate the security for callbacks when
creates the subscription specifying the secret key (`see
more <./services/subscription/subscription.html>`__). This subscription
 should be done using HTTPs protocol to avoid compromising the key.

After the subscription has been created, all the related requests will
include two new headers, one with the hash (:literal:`X-Sentilo-Content-Hmac`)
and another with the timestamp (:literal:`X-Sentilo-Date`), as the following
sample shows:

::

   X-Sentilo-Content-Hmac:
   j1OQ+fU667GQoHYHWzLBpigRjLJmRvYn53KHZhApTbrcphYWBlRPSBHkntODuqsqx11Vj8rsc7DDziiutTq/5g==
   X-Sentilo-Date: 10/06/2019T15:27:22

The responsibility of validating the headers will be always in the
target system who is receiving the messages.

Code Samples
~~~~~~~~~~~~

A simple NodeJS example that would check the authenticity of the message would be:

.. sourcecode:: javascript

   const crypto = require('crypto');
   
   const message = '{"message":"26","timestamp":"03/12/2020T07:36:27","topic":"/data/TITAN/TITAN-S01","type":"DATA","sensor":"TITAN-S01","provider":"TITAN","time":1606980987614,"publisher":"TITAN","publishedAt":1606980987614,"publisherTenant":"","tenant":"","sender":"TITAN"}'
   const endpoint = 'http://my.endpoint.com:1880/sentilo';
   const secretKey = 'my_super_secret_key';
   const headerXSentiloDate = '03/12/2020T07:36:27';
   const headerXSentiloContentHmacValue = 'elMiy5BDgDB68UVMonNDCc/BH8YrLWtCP6CdvlB4T//uI87JmMvx+epPUDy8E3Rg4UC2Bm21n4Zj/CLxOEcEZA==';
   
   // Step 1 - hash the message and finally base64
   let md5body = crypto.createHash('md5').update(message).digest('base64');
   
   
   // Step 2 - concatenate all the necessary values
   let values = ['POST', md5body, 'application/json', headerXSentiloDate, endpoint];
   let contentToSign = values.join('\n');
   
   
   // Step 3 - HMAC and and finally base64
   let hmac = crypto.createHmac('sha512', secretKey);
   hmac.update(contentToSign);
   let result = hmac.digest('base64')
   
   
   // Finally compare with the X-Sentilo-Content-Hmac header
   console.log(result == headerXSentiloContentHmacValue);



Alternatively, another validation example in Java:


.. sourcecode:: java

   import javax.crypto.Mac;
   
   import org.apache.commons.codec.binary.Base64;
   import org.apache.commons.codec.digest.DigestUtils;
   import org.apache.commons.codec.digest.HmacAlgorithms;
   import org.apache.commons.codec.digest.HmacUtils;
   
   
   public class HmacHeaderExample {
   
   
     public static void main(final String[] args) {
   
       // incoming message
       final String body =
           "{\"message\":\"26\",\"timestamp\":\"03/12/2020T07:36:27\",\"topic\":\"/data/TITAN/TITAN-S01\",\"type\":\"DATA\",\"sensor\":\"TITAN-S01\",\"provider\":\"TITAN\",\"time\":1606980987614,\"publisher\":\"TITAN\",\"publishedAt\":1606980987614,\"publisherTenant\":\"\",\"tenant\":\"\",\"sender\":\"TITAN\"}";
       // You're on this endpoint
       final String endpoint = "http://my.endpoint.com:1880/sentilo";
       // Same secret key as the secretCallbackKey in the subscription
       final String secretKey = "my_super_secret_key";
       // Value of X-Sentilo-Date
       final String headerXSentiloDate = "03/12/2020T07:36:27";
       // Value of X-Sentilo-Content-Hmac
       final String headerXSentiloContentHmacValue = "elMiy5BDgDB68UVMonNDCc/BH8YrLWtCP6CdvlB4T//uI87JmMvx+epPUDy8E3Rg4UC2Bm21n4Zj/CLxOEcEZA==";
   
         // Step 1 - hash the message and finally base64
         final byte[] md5BodyDigest = DigestUtils.md5(body);
         final String md5Body = new String(Base64.encodeBase64(md5BodyDigest));
         // Result expected: cIQCRRWeo0yQQLS8rlOtLQ==
   
         // Step 2 - concatenate all the necessary values
         final String[] values = {"POST", md5Body, "application/json", headerXSentiloDate, endpoint};
         final String contentToSign = String.join("\n", values);
         // Result expected
         // POST
         // cIQCRRWeo0yQQLS8rlOtLQ==
         // application/json
         // 03/12/2020T07:36:27
         // http://my.endpoint.com:1880/sentilo
   
         // Step 3 - HMAC and and finally base64
         final Mac mac = HmacUtils.getInitializedMac(HmacAlgorithms.HMAC_SHA_512, secretKey.getBytes());
         final byte[] rawHmac = mac.doFinal(contentToSign.getBytes());
         final String signature = new String(Base64.encodeBase64(rawHmac));
   
         // Finally compare with the X-Sentilo-Content-Hmac header
         System.out.println(signature.equals(headerXSentiloContentHmacValue));
   
     }
   }
