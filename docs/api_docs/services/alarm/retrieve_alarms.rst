Retrieve alarms
===============

Description
-----------

This action allows to retrieve the latest alarms related with an alert.
In addition, the service can also specify search criterias to retrieve
alarms: filter by a given time period and/or indicate the maximum number
of alarms to be retrieved.

::

   http://<your_api_server.com>/alarm/<alarm_id>?<parameter>=<value>

+----------------+----------------------------------+
| **Format**     | json                             |
+----------------+----------------------------------+
| **Method**     | GET                              |
+----------------+----------------------------------+
| **Permission** | Reading                          |
+----------------+----------------------------------+
| **Return**     | Alarms associated with the alert |
+----------------+----------------------------------+

Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| from                  | Indicates the         | Yes                   |
|                       | starting of the time  |                       |
|                       | period for which you  |                       |
|                       | want to retrieve      |                       |
|                       | alarms.               |                       |
+-----------------------+-----------------------+-----------------------+
| to                    | Indicates the end of  | Yes                   |
|                       | the time period for   |                       |
|                       | which you want to     |                       |
|                       | retrieve alarms..     |                       |
+-----------------------+-----------------------+-----------------------+
| limit                 | Specifies the maximum | Yes                   |
|                       | number of alarms to   |                       |
|                       | recover.              |                       |
+-----------------------+-----------------------+-----------------------+

Please note the following:

-  The maximum number of records returned is defined in the platform
   configuration. If the limit parameter has a higher value than the
   configured one it will be dismissed.
-  If limit parameter is not specified, it returns only one alarm.
-  All dates must follow the format: dd/MM/yyyyTHH:mm:ss

Response data
-------------

In addition to the appropriate `HTTP status
code <../../general_model.html#reply>`__, if the operation runs
properly, it will return the last alarms associated with the alert
according to your search criteria.

+--------+--------------------------------------+----------+
| Key    | Description                          | Optional |
+========+======================================+==========+
| alarms | Alarms list (*message*) of the alert | Not      |
+--------+--------------------------------------+----------+

Each alarm (message) will be composed by the following attributes:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Opcional              |
+=======================+=======================+=======================+
| message               | Message recorded when | No                    |
|                       | the alarm was fired   |                       |
+-----------------------+-----------------------+-----------------------+
| timestamp             | The time in which     | No                    |
|                       | system received the   |                       |
|                       | alarm (format         |                       |
|                       | dd/MM/yyyyTHH:mm:ss)  |                       |
+-----------------------+-----------------------+-----------------------+
| time                  | The time when the     | No                    |
|                       | observation was made  |                       |
|                       | in milliseconds       |                       |
+-----------------------+-----------------------+-----------------------+
| sender                | Identifier of the     | No                    |
|                       | entity that issued    |                       |
|                       | the alarm             |                       |
+-----------------------+-----------------------+-----------------------+

Examples
--------

Retrieve the last alarm
~~~~~~~~~~~~~~~~~~~~~~~

To retrieve the latest alarm for the alert with ID 43 we do the
following request to the platform:

::

   http://<your_api_server.com>/alarm/43

In the response we will receive:

.. code:: json

   { "alarms":[
      {
           "message":"threshold exceeded",
           "timestamp":"08/04/2013T09:44:01",
           "time":1510561800008,
           "sender":"appDemo"
       }
   ]}

Recover N alarms
~~~~~~~~~~~~~~~~

To retrieve the last 3 alarms for the alert with id 43 we do the
following request to the platform:

::

   http://<your_api_server.com>/alarm/43?limit=3

In the response we will receive:

.. code:: json

   {"alarms":[
      {
           "message":"threshold exceeded: 34",
           "timestamp":"08/04/2013T09:44:01",
           "time":1510561800000,
           "sender":"appDemo"
       },
      {
           "message":"threshold exceeded: 37",
           "timestamp":"08/04/2013T09:14:01",
           "time":1510561800001,
           "sender":"appDemo"
       },
      {
           "message":"threshold exceeded: 38",
           "timestamp":"07/04/2013T23:23:10",
           "time":1510561800002,
           "sender":"appDemo"
       }
   ]}

Retrieve N alarms in a given period
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to retrieve the alarms according to a given period of time we
should do the following request:

::

   http://<your_api_server.com>/alarm/43?limit=3&from=08/04/2013T00:00:00&to=08/04/2013T23:59:59

In response the we will receive:

.. code:: json

   {"alarms":[
      {
           "message":"threshold exceeded: 34",
           "timestamp":"08/04/2013T09:44:01",
           "time":1510561800000,
           "sender":"appDemo"
       },
      {
           "message":"threshold exceeded: 37",
           "timestamp":"08/04/2013T09:14:01",
           "time":1510561800000,
           "sender":"appDemo"
       }
   ]}
