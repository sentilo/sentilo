Retrieve active subscriptions
=============================

Description
-----------

This action allows to retrieve the list of all our active subcriptions.
Additionally, we can retrieve only subscriptions from a specific type.

::

   http://<your_api_server.com>/subscribe/<event_type>

+----------------+----------------------+
| **Format**     | json                 |
+----------------+----------------------+
| **Method**     | GET                  |
+----------------+----------------------+
| **Permission** | Read                 |
+----------------+----------------------+
| **Returns**    | Active subscriptions |
+----------------+----------------------+


**<event_type>** is optional and allows to filter the subscription by type.

Parameters
----------

No additional parameters can be used.

Response data
-------------

This action, additionally to the `HTTP status
code <../../general_model.html#reply>`__, will return a list of our
active subscriptions:

+---------------+----------------------------------------+----------+
| Key           | Description                            | Optional |
+===============+========================================+==========+
| subscriptions | List with all our active subscriptions | No       |
+---------------+----------------------------------------+----------+


Each **subscription** element contains this set of attributes:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| endpoint              | URL defined in the    | No                    |
|                       | subscription          |                       |
+-----------------------+-----------------------+-----------------------+
| type                  | Event type related to | No                    |
|                       | the                   |                       |
|                       | subscription(data,    |                       |
|                       | order o alarm)        |                       |
+-----------------------+-----------------------+-----------------------+
| provider              | In case the type is   | Yes                   |
|                       | *data* or *order*     |                       |
|                       | this attribute        |                       |
|                       | contains the provider |                       |
|                       | identifier            |                       |
+-----------------------+-----------------------+-----------------------+
| sensor                | In case the type is   | Yes                   |
|                       | *data* or *order*     |                       |
|                       | this attribute        |                       |
|                       | contains the sensor   |                       |
|                       | identifier            |                       |
+-----------------------+-----------------------+-----------------------+
| alarm                 | In case the type is   | Yes                   |
|                       | *alarm* this          |                       |
|                       | attribute contains    |                       |
|                       | the alert identifier  |                       |
+-----------------------+-----------------------+-----------------------+


Examples
--------

Request to retrieve all active subscriptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   http://<your_api_server.com>/subscribe

As response we will obtain:

.. code:: json

   {
      "subscriptions":
      [{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"ALARM",
         "alert":"alerta1"
      },{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"DATA",
         "provider":"app_demo_provider",
         "sensor":"appdemo_sensor5_test"
      },{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"DATA",
         "provider":"app_demo_provider",
         "sensor":"appdemo_sensor_test"
      },{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"ALARM","alert":"11"
      }]
   }

Request to retrieve active subscriptions for a specific type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to retrieve only the subscriptions to a specific event type:

::

   http://<your_api_server.com>/subscribe/alarm

As response we will obtain:

.. code:: json

   {
      "subscriptions":
      [{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"ALARM",
         "alert":"alert1"
      },{
         "endpoint":"http://<your_endpoint_notification_server.com>",
         "type":"ALARM",
         "alert":"alert11"
      }]
   }
