Retrieve orders
===============

Description
-----------

This action allows you to retrieve the last orders associated with a
sensor or provider. In addition,we can also specify search criteria to
retrieve the orders: filter by a given time period and/or indicate the
maximum number of orders that you want to retrieve.

::

   http://<your_api_server.com>/order/<provider_id>/<sensor_id>?<parameter>=<value>

+----------------+------------------------------------------------------+
| **Format**     | json                                                 |
+----------------+------------------------------------------------------+
| **Method**     | GET                                                  |
+----------------+------------------------------------------------------+
| **Permission** | Read                                                 |
+----------------+------------------------------------------------------+
| **Retorns**    | List of orders destined to sensor or provider listed |
+----------------+------------------------------------------------------+


Parameters
----------

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| from                  | Indicates the         | Yes                   |
|                       | beginning of the time |                       |
|                       | period for which you  |                       |
|                       | want to retrieve      |                       |
|                       | orders.               |                       |
+-----------------------+-----------------------+-----------------------+
| to                    | Indicates the ending  | Yes                   |
|                       | of the time period    |                       |
|                       | for which you want to |                       |
|                       | retrieve orders.      |                       |
+-----------------------+-----------------------+-----------------------+
| limit                 | Specifies the maximum | Yes                   |
|                       | number of orders to   |                       |
|                       | retrieve.             |                       |
+-----------------------+-----------------------+-----------------------+


Please, note the following:

-  The maximum number of records returned will be fixed by the platform
   settings. If the parameter passed is higher, the number of records
   returned will be the configured in the platform.
-  If the limit parameter is not set, only one record will be returned.
-  All dates must have the following format: dd/MM/yyyyTHH:mm:ss

Response data
-------------

As mentioned, in addition to `HTTP status
code <../../general_model.html#reply>`__, the requested data is returned
in the body contents as a list of orders.

**The response structure depends on what we are retrieving, orders from
a sensor or a provider.**

Last orders for a sensor
~~~~~~~~~~~~~~~~~~~~~~~~

+--------+-----------------------------------+----------+
| Key    | Description                       | Optional |
+========+===================================+==========+
| orders | List with the last sensor's order | No       |
+--------+-----------------------------------+----------+


Each order will have the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| order                 | Order message         | No                    |
|                       | recorded at the time  |                       |
|                       | the order was         |                       |
|                       | published             |                       |
+-----------------------+-----------------------+-----------------------+
| timestamp             | The time when the     | No                    |
|                       | order was made        |                       |
|                       | (dd/MM/yyyyTHH:mm:ss  |                       |
|                       | format)               |                       |
+-----------------------+-----------------------+-----------------------+
| sender                | Entity identifier     | No                    |
|                       | that issued the       |                       |
|                       | order.                |                       |
+-----------------------+-----------------------+-----------------------+
| time                  | The time when the     | No                    |
|                       | observation was made  |                       |
|                       | in milliseconds       |                       |
+-----------------------+-----------------------+-----------------------+

Last orders for provider
~~~~~~~~~~~~~~~~~~~~~~~~

+---------+----------------------------+----------+
| Key     | Description                | Optional |
+=========+============================+==========+
| sensors | List with sensors (sensor) | No       |
+---------+----------------------------+----------+


Each **(sensor)** will have the following structure:

+--------+------------------------------------------+----------+
| Key    | Description                              | Optional |
+========+==========================================+==========+
| sensor | Sensor identifier                        | No       |
+--------+------------------------------------------+----------+
| orders | List with the last orders for the sensor | No       |
+--------+------------------------------------------+----------+


Finally, each command **(order)** will have the structure that we have
defined previously.

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| order                 | Order message         | No                    |
|                       | recorded at the time  |                       |
|                       | the order was         |                       |
|                       | published             |                       |
+-----------------------+-----------------------+-----------------------+
| timestamp             | The time when the     | No                    |
|                       | order was made        |                       |
|                       | (dd/MM/yyyyTHH:mm:ss  |                       |
|                       | format)               |                       |
+-----------------------+-----------------------+-----------------------+
| sender                | Entity identifier     | No                    |
|                       | that issued the       |                       |
|                       | order.                |                       |
+-----------------------+-----------------------+-----------------------+


Examples
--------

Retrieve the last order for a sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To retrieve the last order for the sensor with RE0012 identifier
belonging to the provider named rec, we do the following request:

::

   http://<your_api_server.com>/order/rec/RE0012

As response we will get:

.. code:: json

   {"orders":[{
      "order":"Shutdown",
      "timestamp":"21/03/2013T14:25:39",
      "sender":"app_demo_provider"}]
   }

Retrieve the last N orders for a sensor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to retreive more than one order, we can specify the number of
records to retrieve with the following request:

::

   http://<your_api_server.com>/order/rec/RE0012?limit=3

As response we will get:

.. code:: json

   {"orders":
      [{
         "order":"Shutdown",
         "timestamp":"21/03/2013T14:25:39",
         "sender":"app_demo_provider",
         "time":1510570798597
      },{
         "order":"Start",
         "timestamp":"20/03/2013T23:59:59",
         "sender":"app_demo_provider",
         "time":1510570798597
      },{
         "order":"Shutdown",
         "timestamp":"20/03/2013T14:25:39",
         "sender":"app_demo_provider",
         "time":1510570798597
      }
   ]}

Retrieve the last N orders for a sensor between dates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to retrieve orders for a sensor between two dates, we should
do the following request:

::

   http://<your_api_server.com>/order/rec/RE0012?limit=3&from=19/03/2013T00:00:00&to=20/03/2013T23:59:59

As response we will get:

.. code:: json

   {"orders":
      [{
         "order":"Start",
         "timestamp":"20/03/2013T23:59:59",
         "sender":"app_demo_provider",
         "time":1510570798597
      },{
         "order":"Shutdown",
         "timestamp":"20/03/2013T14:25:39",
         "sender":"app_demo_provider",
         "time":1510570798597
      }
   ]}

Retrieve the last orders for a provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All the previous examples are focused on recovering the last command of
a sensor, but the service also allows you to search the latest orders
destined for all the sensors of provider.

In this case, we only specify the provider, and the request will be:

::

   http://<your_api_server.com>/order/rec2

As response we get a list of sensor elements, and each one will contain
its last orders.

.. code:: json

   {"sensors":
      [{
         "sensor":"RE0012",
         "orders":
         [{
            "order":"Shutdown",
            "timestamp":"21/03/2013T14:25:39",
            "sender":"app_demo_provider",
            "time":1510570798597
         }]
      },{
         "sensor":"RE0013",
         "orders":
         [{
            "order":"Shutdown",
            "timestamp":"21/03/2013T14:25:39",
            "sender":"app_demo_provider",
            "time":1510570798597
         }]
      },{
         "sensor":"RE0014",
         "orders":
         [{
            "order":"Shutdown",
            "timestamp":"21/03/2013T14:25:39",
            "sender":"app_demo_provider",
            "time":1510570798597
         }]
      }]
   }
