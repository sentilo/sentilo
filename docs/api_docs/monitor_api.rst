Monitor API (Beta)
==================

Since v1.9, Sentilo offers an internal Monitor API that enables direct access to
select real-time information of the API server and other components.
With this internal API you can also remotely restart API server, for example in case the main API is too busy.

The Monitor API runs on a different port then public API. The default port number is :literal:`7081` and is configured in
:literal:`/sentilo/sentilo-platform/sentilo-platform-server/src/main/resources/properties/config.properties`
by property :literal:`monitor.port`:

..

    monitor.port=7081

This API currently works with the Catalog Application token. You can find the catalog token in the Application
section of the Catalog.

.. note::

   The catalog application token is very sensitive information, since it is used for admin operations. Make sure
   you do store it in a private repository. Also make sure calls to the monitoring API are encrypted. See more in the
   `Security section <./security.html>`__


List of API resources in the monitoring API:

+--------------------------+--------+-----------------------------------------------------------------------------------+
|         Resource         | Method |                                    Description                                    |
+==========================+========+===================================================================================+
| /monitor/ping            | GET    | Simple service that responds with http code 200  if the server is up and running. |
+--------------------------+--------+-----------------------------------------------------------------------------------+
| /monitor/rl_input_status | GET    | Returns values of global rate limiting (incoming requests), for the last hour.    |
+--------------------------+--------+-----------------------------------------------------------------------------------+
| /monitor/metrics         | GET    | Returns a list of components with their metrics                                   |
+--------------------------+--------+-----------------------------------------------------------------------------------+
| /monitor/restart         | POST   | Gracefully restarts the API server. No body needed.                               |
+--------------------------+--------+-----------------------------------------------------------------------------------+
| /monitor/force-restart   | POST   | Forces a restart of the API server. No body needed.                               |
+--------------------------+--------+-----------------------------------------------------------------------------------+

