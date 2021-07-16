Subscription
============

.. toctree::
   :hidden:
   :titlesonly:
   :maxdepth: 1
   
   retrieve_sensor_observations
   retrieve_sensor_orders
   retrieve_alerts
   retrieve_subscription_list
   cancel_subscription
   

Description
-----------

The subscription service allows to the platform
clients(application/modules or provider/sensors) to subscribe to system
events, which can be:

-  **Data:** related to data observations received by the platform
-  **Order:** related to orders received by the platform
-  **Alarm:** related to alarms received by the platform

It is also possible to retrieve the list of active subscriptions or
cancel them.

All requests for this service will have the following format:

::

   http://<your_api_server.com>/subscribe/<event_type>/<resource_id>

where **<resource_id>** identifies the system resource to which the request applies
(providers, sensors or alerts).

Actions
-------

The available actions for this service are:

-  `Subscription to sensor data <./retrieve_sensor_observations.html>`__
-  `Subscription to orders <./retrieve_sensor_orders.html>`__
-  `Subscription to alerts <./retrieve_alerts.html>`__
-  `Retrieve active subscriptions <./retrieve_subscription_list.html>`__
-  `Cancel subscription <./cancel_subscription.html>`__

Notifications
~~~~~~~~~~~~~

As mentioned before, when we subscribe to a system event, the platform
will send us a notification (push process), whenever the event occurs,
through a HTTP POST request to the URL configured with the subscription.

The notification message follows the following structure:

.. code:: json

   {
      "message":"...",
      "timestamp":"...",
      "topic":"...",
      "type":"...",
      "sensor":"...",
      "provider":"...",
      "location":"...",   
      "alert":"...",
      "alertType":"...",
      "time":"...",
      "tenant":"..."
      "publisher":"...",
      "publisherTenant":"...",
      "publishedAt":"..."     
   }

where the following fields are mandatory:

-  message: contains the event information (observation, alarm or order)
-  timestamp: contains the timestamp associated with the event,
   formatted as UTC (dd/MM/yyyy’T’HH:mm:ss).
-  topic: identifies the subscription related to the event.
-  type: identifies the event type (DATA, ORDER or ALARM)
-  time: same as timestamp but expressed as milliseconds

and the following are optional and depend on the event type:

-  sensor: contains the sensor identifier related to the event.
-  provider: contains the provider identifier related to the event.
-  location: only added in observation notifications when the location
   is filled in.
-  sender: this field has been removed in version 1.6. See *publisher*
   field.
-  alert: only added in alarm notifications. Contains the alert
   identifier related to the alarm.
-  alertType: only added in alarm notifications. Contains the alert
   type: INTERNAL or EXTERNAL.
-  retryAttempt: if the delivery of the message fails, this number
   indicates a number of the retries. See for example `how to define
   retries in data
   subscription <./retrieve_sensor_observations.html>`__.
-  publisher: identifies the entity who has published the event.
-  publishedAt: this field differs from *time* field in that it always
   stores the time when the event was published on Sentilo.
-  tenant: only added in multitenant instances. This field identifies
   the tenant to which the event belongs.
-  publisherTenant: only added in multitenant instances. This field
   identifies the tenant to which the publisher belongs.

Here are three different examples of notification:

.. code:: json

   {
      "message":"8",
      "timestamp":"26/10/2016T08:50:33",
      "topic":"/data/app_demo_provider/appdemo_sensor_test",
      "type":"DATA",
      "sensor":"appdemo_sensor_test",
      "provider":"app_demo_provider",
      "location": "41.387172 2.17157",
      "time":1477471833000,
      "retryAttempt": 1,
      "publisher":"app_demo_provider",
      "publishedAt":1477471833000,
      "publisherTenant": "",
      "tenant": "",
      "sender": "app_demo_provider"
   }

.. code:: json

   {
      "message":"Stop",
      "timestamp":"16/10/2013T15:39:11",
      "topic":"/order/app_demo_provider",
      "type":"ORDER",
      "sensor": "TEST_SENSOR_001",
      "provider":"app_demo_provider",
      "time":1477471833000,
      "publisher":"app_demo_provider",
      "publishedAt":1477471833000,
      "publisherTenant": "",
      "tenant": "",
      "sender": "app_demo_provider"
   }

.. code:: json

   {
      "message":"Value greater than 34",
      "timestamp":"16/10/2013T15:40:57",
      "topic":"/alarm/internalAlarmProve",
      "type":"ALARM",
      "sensor":"app_demo",
      "alert":"ALERT_GT14",
      "alertType":"INTERNAL", 
      "publisher": :"sentilo"
      "time":1477471833000,
      "publishedAt":1477471833000
   }

If the subscription has included a secret key, the following messages
will include the **security headers** (`see
more <../../security.html#securityCallbacks>`__).

Notifications to untrusted HTTPS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In case that remote endpoint uses a self-signed certificate, add the
following configuration in the :literal:`config.properties` of the
sentilo-platform-server:

::

   #Allows Sentilo to send notifications to untrusted servers, i.e., servers with self signed certificates or signed by unknown CAs
   api.subs.ssl.no-validate-certificates=false
