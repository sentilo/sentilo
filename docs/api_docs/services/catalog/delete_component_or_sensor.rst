Delete components / sensors
===========================

Description
-----------

This action allows the provider to delete catalog components and/or
sensors.

**WARNING: this operation performs a cascade delete and the execution of
this action cannot be undone.**

::

   http://<your_api_server.com>/catalog/<provider_id>?<parameter>=<value>

+----------------+----------------+
| **Format**     | json           |
+----------------+----------------+
| **Method**     | DELETE, PUT    |
+----------------+----------------+
| **Permission** | Writing        |
+----------------+----------------+
| **Return**     | No output data |
+----------------+----------------+


Note that his action can be invoked using two HTTP methods: PUT and
DELETE.

-  DELETE we be used to delete all the sensors and components of a
   provider. It cannot contain body content.
-  PUT will be used to delete a group of sensors or components. We
   should add a the parameter method with delete value to the request.
   In this case, the sensors or components to delete should be specified
   in the body message.

Parameters
----------

The structure of the input parameters depends on whether you want to
delete components or sensors.

The following describes the structure of the input in each case:

Delete components
~~~~~~~~~~~~~~~~~

+------------+------------------------------------------+----------+
| Key        | Description                              | Optional |
+============+==========================================+==========+
| components | Array of component identifiers to delete | Yes      |
+------------+------------------------------------------+----------+


Each element of the list corresponds to an identifier of a component to
delete.

Delete sensors
~~~~~~~~~~~~~~

+---------+---------------------------------------+----------+
| Key     | Description                           | Optional |
+=========+=======================================+==========+
| sensors | Array of sensor identifiers to delete | Yes      |
+---------+---------------------------------------+----------+


Each element of the list corresponds to an identifier of a sensor to
delete.

Response data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to delete all components and sensors of a provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To delete all components and sensors belonging to the provider named rec
the request to do is the following:

::

   DELETE http://<your_api_server.com>/catalog/rec

This request will delete in the catalog all the components and sensors
of the rec provider

Request to delete a set of components of the catalog
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To delete a set of components belonging to the provider rec the request
to do is the following:

::

   PUT http://<your_api_server.com>/catalog/rec?method=delete

in the body message:

.. code:: json

   {"components":["COMP-3","COMP-4"]}

Request to delete a set of sensors of the catalog
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To delete a set of sensors belonging to the provider rec the request to
do is the following:

::

   PUT http://<your_api_server.com>/catalog/rec?method=delete

in the body message:

.. code:: json

   {"sensors":["RE001","RE002","RE003"]}
