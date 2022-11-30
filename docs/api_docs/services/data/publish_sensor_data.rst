Publish observations from a sensor
==================================

Description
-----------

This action allows a provider to publish the observations made by one of
its sensors.

::

   http://<your_api_server.com>/data/<provider_id>/<sensor_id>

+----------------+----------------+
| **Formats**    | json           |
+----------------+----------------+
| **Method**     | PUT            |
+----------------+----------------+
| **Permission** | Writing        |
+----------------+----------------+
| **Retorna**    | No output data |
+----------------+----------------+


Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| observations          | Observations list to  | No                    |
|                       | publish.              |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Geolocation           | Yes                   |
|                       | coordinates in which  |                       |
|                       | the sensor got the    |                       |
|                       | observations(latitude |                       |
|                       | longitude format).    |                       |
+-----------------------+-----------------------+-----------------------+

Each observation will have the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| value                 | Observation value to  | No                    |
|                       | register              |                       |
+-----------------------+-----------------------+-----------------------+
| timestamp             | Date and time when    | Yes                   |
|                       | the observation was   |                       |
|                       | made (format          |                       |
|                       | dd/MM/yyyyTHH:mm:ssZ) |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Geolocation           | Yes                   |
|                       | coordinates, in       |                       |
|                       | decimal degrees, in   |                       |
|                       | which the sensor got  |                       |
|                       | the                   |                       |
|                       | observations(latitude |                       |
|                       | longitude format)     |                       |
+-----------------------+-----------------------+-----------------------+


Please note the following:

-  If you send an observation of a sensor without specifying timestamp,
   the platform will use the current timestamp as measurement time.
-  The location of the observations is optional. But in case you want to
   set it, you can do it for all observations and/or individually for
   each one. The location informed for each observation takes precedence
   over the global localization.
-  The TimeZone (Z) in the timestamps is optional. Its default value is
   UTC.
-  In previous releases (up to 1.5.x) the system permitted publication
   of sensors that weren’t registered in the catalog. Since 1.6, the
   sensor has to be correctly registered.
-  If the sensor is in offline state, the server rejects the
   publication.

Response data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to publish the last observation of a sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To publish an observation of a sensor service provides two ways to do
it.

Abbreviated request
^^^^^^^^^^^^^^^^^^^

If we want to publish the last observation for the sensor with RE0012
identifier belonging to the provider with rec identifier we just have to
add the value to the URL. In this case we can not send timestamp nor
location:

::

    http://<your_api_server.com>/data/rec/RE0012/12.3

where 12.3 is the observation value.

As indicated previously, this request will register a new observation
(value 12.3) for the specified sensor. The timestamp of the observation
will be the instant of arrival of the request to the platform.

Normal Request
^^^^^^^^^^^^^^

If you wish to send the timestamp and the location of the observation
too, we should use the format described, and send information in the
body of the request.

For example, if in we want to include the timestamp of the observation,
the request to do will be the following:

::

    http://<your_api_server.com>/data/rec/RE0012

in the body message

.. code:: json

   {"observations":[{
      "value":"12.3",
      "timestamp":"17/09/2012T12:34:45"}
   ]}

This request will register a new observation(value 12.3) with the
received timestamp (UTC time zone in this case) of the measurement.

Another example: it shows how to publish the temperature measured on
Barcelona at a given time, sending the time in the Barceloca local time
zone (CET):

.. code:: json

   {"observations":[{
      "value":"9.6",
      "timestamp":"17/02/2016T11:43:45CET",
      "location": "41.3888 2.15899"}
   ]}

Request to publish several observations of the same sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want to send more of an observation of a sensor, the request is
very similar to the previous one, only changing the message body.

::

    http://<your_api_server.com>/data/rec/RE0012

in the body message

.. code:: json

   {"observations":[{
       "value":"10.1"
      },{
       "value":"11.2",
       "timestamp":"17/09/2012T12:34:45"
      },{
       "value":"12.3",
       "timestamp":"17/09/2012T10:34:45"
      }
   ]}

In this case are three observations with the corresponding timestamps.
