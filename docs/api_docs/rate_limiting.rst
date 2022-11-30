Rate Limiting
=============

Since v1.9, Sentilo offers rate limiting features for the REST API.

Rate limiting prevents the Sentilo instance from request flooding. Also, protects
 external systems that are subscribed to Sentilo by limiting outcoming requests.

Setting global limits
---------------------

You can set global incoming quota :literal:`api.global_rate_limit.quota` in file
:literal:`sentilo-platform/sentilo-platform-server/src/main/resources/properties/config.properties`.
If left unset or set to 0, Sentilo won't apply any global limit.

The maximum body length can also be set globally with :literal:`api.body.max_length` property.

::

   # number of requests per hour
   api.global_rate_limit.quota=100

   # Maximum body length in bytes
   api.body.max_length=4096


If global rate limiting is set, every response of the API contains following headers:

+--------------------------------------+-----------------------------------------------------+
|                Header                |                     Description                     |
+======================================+=====================================================+
| X-RateLimit-Global-Inbound-Limit     | Value of the api.global_rate_limit.quota property   |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Global-Inbound-Remaining | Number of remaining requests until the current      |
|                                      | limit per hour is exceed                            |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Global-Inbound-Reset     | If the API limit is exceeded, this value indicates  |
|                                      | the number of minutes until the server will accept  |
|                                      | requests again. Otherwise is 0.                     |
+--------------------------------------+-----------------------------------------------------+

In case the global rate limit is exceed, the server will reject the request and return a HTTP error code 429.

In case the maximum payload size is exceed, the server responses with HTTP error code 400.




Limit requests by entity
------------------------

A provider or an application can have particular limits, which can be configured in catalogue:

|rl_entity_190.png|

If input quota for a entity is different from 0, every response of the API contains following headers:

+--------------------------------------+-----------------------------------------------------+
|                Header                |                     Description                     |
+======================================+=====================================================+
| X-RateLimit-Inbound-Limit            | Value of the input quota                            |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Inbound-Remaining        | Number of remaining requests until the current      |
|                                      | limit per hour is exceed                            |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Inbound-Reset            | If the entity quota is exceeded, this value         |
|                                      | indicates the number of minutes until entity will   |
|                                      | be permitted again. Otherwise is 0.                 |
+--------------------------------------+-----------------------------------------------------+

If output quota for a entity is different from 0, every subscription request that Sentilo sends to the
subscription endpoint will contain following headers:

+--------------------------------------+-----------------------------------------------------+
|                Header                |                     Description                     |
+======================================+=====================================================+
| X-RateLimit-Outbound-Limit           | Value of the output quota                           |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Outbound-Remaining       | Number of remaining requests until the current      |
|                                      | limit per hour is exceed                            |
+--------------------------------------+-----------------------------------------------------+
| X-RateLimit-Outbound-Reset           | This value indicates the number of minutes until    |
|                                      | the counter of remaining outgoing requests of the   |
|                                      | entity will be reset. Otherwise is 0.               |
+--------------------------------------+-----------------------------------------------------+

For example, consider that the subscribed system receives a message, together with these headers:

-  X-RateLimit-Outbound-Limit: 5
-  X-RateLimit-Outbound-Remaining: 1
-  X-RateLimit-Outbound-Reset: 58

We can see that this entity that is subscribed to Sentilo data has a output quota set to  5 notifications/hour,
that it can still receive 1 more notification and that the current window is still valid during next 58 minutes.

As with the global limit,if the entity's input quota is exceed, the server will reject the request and return
a HTTP error code 429. Global rate limit is prevalent to entity's quota, so the request is rejected when the lowest of
the two is met.



.. |rl_entity_190.png| image:: /_static/images/api_docs/rl_entity_190.png
