Update Alerts
=============

Description
-----------

This action allows to update one or more alerts in the catalog.

::

   http://<your_api_server.com>/catalog/alert/<entity_id>

+----------------+----------------+
| **Formats**    | json           |
+----------------+----------------+
| **Method**     | PUT            |
+----------------+----------------+
| **Permission** | Writing        |
+----------------+----------------+
| **Returns**    | No output data |
+----------------+----------------+

The internal alerts should be updated through the catalog console or by
the API, but only using the catalog token.

Parameters
----------

+--------+---------------------------------+----------+
| Key    | Description                     | Optional |
+========+=================================+==========+
| alerts | Alerts list (*alert*) to update | Not      |
+--------+---------------------------------+----------+

Every alert element has the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| id                    | Alert identifier to   | No                    |
|                       | update                |                       |
+-----------------------+-----------------------+-----------------------+
| name                  | New alert name        | Yes                   |
+-----------------------+-----------------------+-----------------------+
| description           | New alert description | Yes                   |
+-----------------------+-----------------------+-----------------------+
| type                  | Alert type            | No                    |
+-----------------------+-----------------------+-----------------------+
| trigger               | New trigger type      | Mandatory for         |
|                       |                       | internal, not applies |
|                       |                       | for externals         |
+-----------------------+-----------------------+-----------------------+
| expression            | New expression to     | Mandatory for         |
|                       | evaluate with the     | internal, not applies |
|                       | trigger               | for externals         |
+-----------------------+-----------------------+-----------------------+

Please, note the following observations:

-  The list of trigger’s types and expressions are defined in: `Trigger
   types <../alert/alert.html#InternalTriggerTypes>`__.
-  The possible values ​​for the alert type are: :literal:`INTERNAL` or :literal:`EXTERNAL`.

Response data
-------------

This action doesn’t return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__.

Examples
--------

Update one external alert
~~~~~~~~~~~~~~~~~~~~~~~~~

If rec entity wants to update the external alert with REC_ALERT_001
identifier to modify its name, the request to do will be:

::

   http://<your_api_server.com>/catalog/alert/rec

and in the body message:

.. code:: json

   {"alerts":[
      {"id":"REC_ALERT_001",
       "name":"REC_EXTERNAL_ALERT_001",
       "type":"EXTERNAL"
      }
   ]}

This request will update the external alert with REC_ALERT_001
identifier updating its name to REC_EXTERNAL_ALERT_001.

.. note::

   External alerts are defined by third party
   entities(providers or applications), which will be the responsibles of
   calculating their logic and throw the related alarms when applies.


Update one internal alert
~~~~~~~~~~~~~~~~~~~~~~~~~

If we want to update the internal alert with REC_GT_45_ALERT_001
identifier to change its description, the request will be:

::

   http://<your_api_server.com>/catalog/alert/rec

and in the body message:

.. code:: json

   {"alerts":[
      {"id":"REC_GT_45_ALERT_001",    
       "type":"INTERNAL",
       "description":"New description"
      }
   ]}

This request will update the description of the internal alert with
REC_GT_45_ALERT_001 identifier changing its value to “New description”.

**This operation must be done using the catalog token.**
