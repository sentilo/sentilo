Read observations from provider’s sensors
=========================================

Description
-----------

This action allows to retrieve the latest observations of the sensors of
a provider. In addition, the service can also specify search criterias
to retrieve observations: filter by a given time period and / or to
indicate the maximum number of observations to be recovered.

::

    http://<your_api_server.com>/data/<provider_id>?<parameter>=<value>

+----------------+----------------------------------------------------+
| **Format**     | json                                               |
+----------------+----------------------------------------------------+
| **Method**     | GET                                                |
+----------------+----------------------------------------------------+
| **Permission** | Reading                                            |
+----------------+----------------------------------------------------+
| **Return**     | List with the observations from provider's sensors |
+----------------+----------------------------------------------------+



Parameteres
-----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| from                  | Indicates the         | Yes                   |
|                       | beginning of the time |                       |
|                       | period for which you  |                       |
|                       | want to retrieve the  |                       |
|                       | observations.         |                       |
+-----------------------+-----------------------+-----------------------+
| to                    | Indicates the end of  | Yes                   |
|                       | the time period for   |                       |
|                       | which you want to     |                       |
|                       | retrieve the          |                       |
|                       | observations.         |                       |
+-----------------------+-----------------------+-----------------------+
| limit                 | Specifies the maximum | Yes                   |
|                       | number of             |                       |
|                       | observations for each |                       |
|                       | sensor to recover.    |                       |
+-----------------------+-----------------------+-----------------------+


Please note the following:

-  The maximum number of records returned will be fixed by the platform
   settings. If the parameter passed is higher, the number of records
   returned will be equalsa to the maximum value configured in the
   platform.
-  If the limit parameter is not set, only one record will be returned.
-  All dates must have the following format: dd/MM/yyyyTHH:mm:ss

Response data
-------------

In addition to the `HTTP status
code <../../general_model.html#reply>`__, the observation data is
returned in the body contents as a list of observations:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| sensor                | List of sensors       | No                    |
|                       | (sensor) for the      |                       |
|                       | observations that     |                       |
|                       | have been retrieved   |                       |
+-----------------------+-----------------------+-----------------------+


Each sensor has the following structure:

+--------------+----------------------------------------+----------+
| Key          | Description                            | Optional |
+==============+========================================+==========+
| sensor       | Sensor identifier                      | No       |
+--------------+----------------------------------------+----------+
| observations | List of the latest sensor observations | No       |
+--------------+----------------------------------------+----------+


Finally, each observation (observation) has the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Opional               |
+=======================+=======================+=======================+
| value                 | Observation value     | No                    |
+-----------------------+-----------------------+-----------------------+
| timestamp             | The time at which the | No                    |
|                       | observation was made  |                       |
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

Request to retrieve the latest observations from a provider after a given date
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to retrieve the latest observations of the sensors associated
with the provider named rec from a given date we should make the
following request:

::

    http://<your_api_server.com>/data/rec?from=10/09/2012T10:00:00

As response we will receive:

.. code:: json

   {"sensors":[
      {
         "sensor":"RE0012",
         "observations":
         [{
            "value":"1",
            "timestamp":"10/09/2012T10:05:00",
            "time":1510561800000
         },{
            "value":"1.2",
            "timestamp":"10/09/2012T07:05:00",
            "time":1510561800000
         }]
      },{
         "sensor":"RE0013",
         "observations":
         [{
            "value":"24",
            "timestamp":"10/09/2012T10:06:10",
            "time":1510561800000
         }]
      }
   ]}

Request to retrieve the latest observations from rec provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you only want to retrieve the last observation of the RE0012 sensor,
the request to do is:

::

    http://<your_api_server.com>/data/rec

As response we will receive:

.. code:: json

   {"sensors":[
      {
         "sensor":"RE0012",
         "observations":
         [{
            "value":"1",
            "timestamp":"10/09/2012T10:05:00",
            "time":1510561800000
         }]
      },{
         "sensor":"RE0013",
         "observations":
         [{
            "value":"24",
            "timestamp":"10/09/2012T10:06:10",
            "time":1510561800000
         }]
      }
   ]}
