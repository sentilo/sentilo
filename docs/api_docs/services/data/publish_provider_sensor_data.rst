Publishing observations from different sensors
==============================================

Description
-----------

This action allows a provider to publish details of the observations
made ​​by more than one sensor in a single message.

::

    http://<your_api_server.com>/data/<provider_id>

+----------------+----------------+
| **Format**     | json           |
+----------------+----------------+
| **Method**     | PUT            |
+----------------+----------------+
| **Permission** | Writing        |
+----------------+----------------+
| **Return**     | No output data |
+----------------+----------------+


Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| sensors               | List of sensors       | No                    |
|                       | (sensor) for which we |                       |
|                       | publish at least one  |                       |
|                       | observation           |                       |
+-----------------------+-----------------------+-----------------------+


Each sensor will have the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| sensor                | Sensor identifier     | No                    |
+-----------------------+-----------------------+-----------------------+
| observations          | Observations list     | No                    |
|                       | (*observation*) to    |                       |
|                       | publish               |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Geolocation           | Yes                   |
|                       | coordinates in which  |                       |
|                       | the sensor            |                       |
|                       | observations are      |                       |
|                       | obtained (latitude    |                       |
|                       | longitude format)     |                       |
+-----------------------+-----------------------+-----------------------+


Each observation will have the structure described on page `Publish
observations of a sensor <./publish_sensor_data.html>`__:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| value                 | Observation value     | No                    |
+-----------------------+-----------------------+-----------------------+
| timestamp             | Date and time at      | Yes                   |
|                       | which the observation |                       |
|                       | was made              |                       |
|                       | (dd/MM/yyyyTHH:mm:ssZ |                       |
|                       | format)               |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Geolocation           | Yes                   |
|                       | coordinates in which  |                       |
|                       | the sensor has        |                       |
|                       | achieved this         |                       |
|                       | observation (latitude |                       |
|                       | longitude format).    |                       |
+-----------------------+-----------------------+-----------------------+


Response Data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to send multiple observations of several sensors setting a LTC TimeZone
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to send the observations of a set of sensors for the provider
named rec, setting timeZone to CET, the request to do is:

::

    http://<your_api_server.com>/data/rec

and in the body message:

.. code:: json

   {"sensors":[
      {
       "sensor":"RE0012",
       "observations":[
         {"value":"1.1"},
         {"value":"1.2",
          "timestamp":"17/09/2012T12:34:45CET"},
         {"value":"1.3",
          "timestamp":"17/09/2012T10:34:45CET"}
       ]
      },{
       "sensor":"RE0013",
       "location":"41.12345 2.12354",
       "observations":[
         {"value":"2.1"},
         {"value":"2.2",
          "timestamp":"16/09/2012T15:43:21CET"},
         {"value":"2.3",
          "timestamp":"16/09/2012T10:43:21CET"}
       ]
      }
   ]}
