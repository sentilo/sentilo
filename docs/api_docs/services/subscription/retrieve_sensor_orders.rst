Subscription to orders
======================

Description
-----------

This action allows to subscribe to orders associated to sensors.

**It’s important to note that we only can subscribe to the sensor data
over we own read permission.**

::

   http://<your_api_server.com>/subscribe/order/<provider_id>/<sensor_id>

+----------------+-----------------------------+
| **Formats**    | json                        |
+----------------+-----------------------------+
| **Method**     | PUT                         |
+----------------+-----------------------------+
| **Permission** | Read                        |
+----------------+-----------------------------+
| **Returns**    | No additional data returned |
+----------------+-----------------------------+


Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| endpoint              | URL where the         | No                    |
|                       | platform will send a  |                       |
|                       | HTTP request with the |                       |
|                       | order data            |                       |
+-----------------------+-----------------------+-----------------------+
| secretCallbackKey     | Secret key for HMAC   | Yes                   |
|                       | callbacks             |                       |
+-----------------------+-----------------------+-----------------------+
| maxRetries            | Maximum number of     | Yes                   |
|                       | retries               |                       |
+-----------------------+-----------------------+-----------------------+
| retryDelay            | Delay parameter in    | Yes                   |
|                       | minutes. Delays are   |                       |
|                       | spaced exponentially  |                       |
|                       | according to          |                       |
|                       | following equation:   |                       |
|                       | delay (N) = delay \*  |                       |
|                       | 2^(N-1)               |                       |
|                       | Where N is the        |                       |
|                       | current entry turn.   |                       |
|                       | More detailed         |                       |
|                       | explanation follows.  |                       |
+-----------------------+-----------------------+-----------------------+


**Retries**

In case the remote endpoint is down or does not respond with an success
HTTP 2xx code, Sentilo can try to resend the data later. In order to
overcome major number of remote outages, Sentilo sends the data in delay
times that are exponential according to equation:

::

   delay (N) = delay * 2^(N-1)

For example, if we have a subscription configured with 5 retries and 10
minutes, first retry would occur at 10 minutes, the second 20 minutes
after the first, the third 40 minutes after the second, etc up to the
fifth retry.

The total time used for the 5 retries would occur in 10+20+40+80+160=310
minutes after the first failed intent.

Response data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to subscribe to orders for a sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to susbcribe to the orders for the sensor with RE0012
identifer of the provider named rec, the request will be:

::

   http://<your_api_server.com>/subscribe/order/rec/RE0012

and the body message:

.. code:: json

   {"endpoint":"http://<your_endpoint_notification_server.com>"}

Request to subscribe to orders for a provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to subscribe to all the sensor’s orders belonging to the rec
provider, the request will be:

::

   http://<your_api_server.com>/subscribe/order/rec

and like body message:

.. code:: json

   {"endpoint":"http://<your_endpoint_notification_server.com>"}
