Publish an order
================

Description
-----------

This operation allows to send an order to single sensor or to all
sensors of a provider. Once the system receives the order, it sends a
notification to all its subscribers.

::

   http://<your_api_server.com>/order/<provider_id>/<sensor_id>

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

Each order will have its specific structure with its associated
information in the defined format (JSON).

The platform will only transfer the information to the subscribers,
without checking its contents nor reading into it.

+-------+---------------+----------+
| Key   | Description   | Optional |
+=======+===============+==========+
| order | Orden content | Not      |
+-------+---------------+----------+


Response data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Publish an order to a sensor/actuator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following example shows how to send a request to the platform to
publish a new order destined to the sensor with RE0012 identifier
belonging to the provider with ID rec:

::

   http://<your_api_server.com>/order/rec/RE0012

in the body message:

.. code:: json

   {"order":"Stop"}

Publish an order to all the provider’s sensors/actuators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following example shows how to send a request to the platform to
publish a new order to all the sensors belonging to the provider with
rec identifier:

::

   http://<your_api_server.com>/order/rec

in the body message

.. code:: json

   {"order":"Start RE0012, RE0013"}
