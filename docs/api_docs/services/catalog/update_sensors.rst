Update data of a component / sensor
===================================

Description
-----------

This action permits to update the catalog information related to
components and/or sensors of a provider.

::

    http://<your_api_server.com>/catalog/<provider_id> 

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

The structure of the input parameters depends on what we want to modify,
sensor or component data.

The following describes the structure of the input parameters in each
case:

Update components
~~~~~~~~~~~~~~~~~

+------------+-----------------------------------------+----------+
| Key        | Description                             | Optional |
+============+=========================================+==========+
| components | Components list (*component*) to update | Yes      |
+------------+-----------------------------------------+----------+

Each element **component** has the following structure:

+---------------------------+-----------------------------------------+----------+
| Key                       | Description                             | Optional |
+===========================+=========================================+==========+
| component                 | Component ID to update                  | No       |
+---------------------------+-----------------------------------------+----------+
| componentType             | Component type                          | Yes      |
+---------------------------+-----------------------------------------+----------+
| componentDesc             | Component description                   | Yes      |
+---------------------------+-----------------------------------------+----------+
| location                  | Component location/s                    | Yes      |
+---------------------------+-----------------------------------------+----------+
| componentPublicAccess     | Visualization check for the public area | Yes      |
+---------------------------+-----------------------------------------+----------+
| componentAdditionalInfo   | Additional params                       | Yes      |
+---------------------------+-----------------------------------------+----------+
| componentTechnicalDetails | Technical params                        | Yes      |
+---------------------------+-----------------------------------------+----------+


The constraints and validation for the parameters are the same as
described in `Adding sensors or components <./create_sensors.html>`__.

Update sensors
~~~~~~~~~~~~~~

+---------+-----------------------------------+----------+
| Key     | Description                       | Optional |
+=========+===================================+==========+
| sensors | Sensors list (*sensor*) to update | Yes      |
+---------+-----------------------------------+----------+


Each **sensor** element has the following structure:


+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
|       Key         |                                                         Description                                                           | Optional |
+===================+===============================================================================================================================+==========+
| sensor            | Sensor ID to update                                                                                                           | No       |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| description       | Sensor description                                                                                                            | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| type              | Sensor type                                                                                                                   | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| dataType          | Data type of the sensor                                                                                                       | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| unit              | Measurement unit                                                                                                              | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| publicAccess      | Visualization check for the public area                                                                                       | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| additionalInfo    | Additional params                                                                                                             | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| technicalDetails  | Technical params                                                                                                              | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| ttl               | Time in minutes when sensor data will expire. If not set, the value of :literal:`redis.expire.data.seconds` will be applied.  | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+
| state             | State of the sensor, either :literal:`online` or :literal:`offline`                                                           | Yes      |
+-------------------+-------------------------------------------------------------------------------------------------------------------------------+----------+




The constraints and validation for the parameters are the same as
described in `Adding sensors or components <./create_sensors.html>`__.

Response data
-------------

This action doesn’t return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to update the sensor data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want to modify the sensor’s description fot the identifiers
RE0012 and RE0013, from rec provider, the request will be:

::

    http://<your_api_server.com>/catalog/rec

in the body message:

.. code:: json

   {"sensors":[
      {"sensor":"REC012","description":"sensor 12"},
      {"sensor":"REC013","description":"sensor 13"}
   ]}

This request will update the description of the sensors RE0012 and
RE0013.

Note: If you need to move a sensor to another component, it should be
done by deleting the sensor and creating it again in the other
component.

Request to update the component data
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to update component data of a provider, like update its
location and additional info, the request will be:

::

    http://<your_api_server.com>/catalog/rec

in the message body:

.. code:: json

   {"components":[
      {"component":"COMP-2","location":"41.4051143 2.1320120","componentAdditionalInfo":{"altitude":"530 m."}}
   ]}
