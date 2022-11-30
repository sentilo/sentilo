Retrieve sensor observations
============================

Description
-----------

This action allows you to retrieve the latest observations of a sensor.
In addition, the service can also permits to specify search criteria to
retrieve observations: filter by a given time period and / or to
indicate the maximum number of observations to be retrieved.

::

    http://<your_api_server.com>/data/<provider_id>/<sensor_id>?<parameter>=<value>

+----------------+-------------------+
| **Format**     | json              |
+----------------+-------------------+
| **Method**     | GET               |
+----------------+-------------------+
| **Permission** | Reading           |
+----------------+-------------------+
| **Returns**    | Observations list |
+----------------+-------------------+


Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| from                  | Indicates the         | Yes                   |
|                       | beginning of the time |                       |
|                       | period for which you  |                       |
|                       | want to retrieve      |                       |
|                       | observations          |                       |
+-----------------------+-----------------------+-----------------------+
| to                    | Indicates the end of  | Yes                   |
|                       | the time period for   |                       |
|                       | which you want to     |                       |
|                       | retrieve observations |                       |
+-----------------------+-----------------------+-----------------------+
| limit                 | Indicates the number  | Yes                   |
|                       | of observations to    |                       |
|                       | retrieve              |                       |
+-----------------------+-----------------------+-----------------------+


Please, note the following:

-  The maximum number of records returned will be fixed by the platform
   settings. If the parameter passed is higher, the number of records
   returned will be equals to the maximum value configured in the
   platform.
-  If the limit parameter is not set, only one observation will be
   returned.
-  All dates must have the following format: dd/MM/yyyyTHH:mm:ssZ with Z
   as optional (and with default value UTC)

Response data
-------------

As mentioned, in addition to `HTTP status
code <../../general_model.html#reply>`__, the observation data is
returned in the body contents as a list of observations:

+--------------+-------------------------------------+----------+
| Key          | Description                         | Optional |
+==============+=====================================+==========+
| observations | List the observations (observation) | No       |
+--------------+-------------------------------------+----------+


Each observation has the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| value                 | Observation value     | No                    |
+-----------------------+-----------------------+-----------------------+
| timestamp             | The time when the     | No                    |
|                       | observation was made  |                       |
|                       | based on UTC          |                       |
|                       | (dd/MM/yyyyTHH:mm:ss  |                       |
|                       | format)               |                       |
+-----------------------+-----------------------+-----------------------+
| time                  | The time when the     | No                    |
|                       | observation was made  |                       |
|                       | in milliseconds       |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Geolocation           | Yes                   |
|                       | coordinates in which  |                       |
|                       | the sensor was        |                       |
|                       | recorded observation  |                       |
+-----------------------+-----------------------+-----------------------+


Examples
--------

Request to retrieve the latest observations of a sensor based on a date
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following request shows an example in which a call is made to
retrieve the last 20 observations of the sensor with RE0012 identifier
of the provider named rec which have been registered from 10/01/2013.

::

    http://<your_api_server.com>/data/rec/RE0012?limit=20&from=10/01/2013T10:00:00

As response we receive:

.. code:: json

   {"observations":[
      {
         "value":"28.61132406103821",
         "timestamp":"13/11/2017T09:00:00",
         "time":1510563600000

      },{
         "value":"20.795568440010314",
         "timestamp":"13/11/2017T08:30:00",
         "time":1510561800000
      },{
         "value":"91.01094902496055",
         "timestamp":"13/11/2017T08:30:00",
         "time":1510561800000
      },{
         "value":"62.22915604583776",
         "timestamp":"11/01/2013T08:16:38",
         "time":1510561800000
      },{
         "value":"99.96065618303348",
         "timestamp":"11/01/2013T07:16:38",
         "time":1510561800000
      },{
         "value":"94.95685904585568",
         "timestamp":"11/01/2013T06:16:38",
         "time":1510561800000
      },{
         "value":"51.26506022800391",
         "timestamp":"11/01/2013T05:16:38",
         "time":1510561800000
      },{
         "value":"21.43303677241535",
         "timestamp":"11/01/2013T04:16:38",
         "time":1510561800000
      },{
         "value":"55.6601921120059",
         "timestamp":"11/01/2013T03:16:38",
         "time":1510561800000
      },{
         "value":"56.692086830598996",
         "timestamp":"11/01/2013T02:16:38",
         "time":1510561800000
      }
   ]}

Request to retrieve the last observation of a sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you only want to retrieve the last observation of the RE0012 sensor,
the request to do is:

::

    http://<your_api_server.com>/data/rec/RE0012

As response we will receive:

.. code:: json

   {"observations":[{
      "value":"11.5",
      "timestamp":"18/09/2012T17:20:00",
      "time":1510561800000}
   ]}
