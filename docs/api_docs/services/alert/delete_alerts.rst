Remove alerts
=============

Description
-----------

This action allows to delete alerts from the catalog. The internal
alerts can only be deleted using the Catalog’s token or through the
Catalog console. The external alerts can only be removed using the
entity’s owner token.

::

   http://<your_api_server.com>/catalog/alert/<entity_id>

.. note::

   entity_id can be also an Application or a Provider too.

+----------------+----------------+
| **Format**     | json           |
+----------------+----------------+
| **Method**     | DELETE, PUT    |
+----------------+----------------+
| **Permission** | Writing        |
+----------------+----------------+
| **Return**     | No output data |
+----------------+----------------+

Note that this action can be invoked using two HTTP methods: PUT and
DELETE.

-  DELETE will be used if we want to delete all of our alerts. It cannot
   contain any body content.
-  PUT will be used when we want to delete a group of alerts. We should
   add the parameter method with delete value to the request . In this
   case, the alerts to delete should be specified in the body message.

Parameters
----------

The structure of input message if we want to delete a group is:

+-----------+-------------------------------------------+----------+
| Key       | Description                               | Optional |
+===========+===========================================+==========+
| alertsIds | Array of the alerts identifiers to delete | Yes      |
+-----------+-------------------------------------------+----------+

Each element of the list corresponds to an identifier to an alert to
delete.

Response data
-------------

This action does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Request to delete all alerts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the entity rec wants to delete all its alerts, the request will be:

::

   DELETE http://<your_api_server.com>/catalog/alert/rec

This action will delete all the external alerts belonging to entity rec.
Be careful, if this request is done using the catalog token, it will
remove all the internal alerts!.

Request to delete a set of alerts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the entity rec only wants to delete a set of alerts, the request will
be:

::

   PUT http://<your_api_server.com>/catalog/alert/rec?method=delete

and in the body message:

::

   {"alertsIds":["REC-ALERT-01","REC-ALERT-02"]}
