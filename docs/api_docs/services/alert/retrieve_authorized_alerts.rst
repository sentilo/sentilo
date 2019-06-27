Retrieve Authorized Alerts
==========================

Description
-----------

This action returns the list of alerts for which the entity_id could do
a subscription, i.e., alerts that belongs to entity_id or alerts for
which entity_id has read permission over its owner. In addition, the
service also allows you to specify search criteria to filter alerts to
be retrieved: filter by alert type and / or filter by trigger type.

::

    http://<your_api_server.com>/catalog/alert/<entity_id>?<parameter>=<value>

The entity_id is optional and can be an Application or a Provider.

+----------------+---------------------------+
| **Format**     | json                      |
+----------------+---------------------------+
| **Method**     | GET                       |
+----------------+---------------------------+
| **Permission** | Reading                   |
+----------------+---------------------------+
| **Return**     | List of authorized alerts |
+----------------+---------------------------+

Parameters
----------

+---------+-----------------------+----------+
| Key     | Description           | Optional |
+=========+=======================+==========+
| type    | Alert's type filter   | Yes      |
+---------+-----------------------+----------+
| trigger | Trigger's type filter | Yes      |
+---------+-----------------------+----------+

Please, note the following observations:

-  The list of trigger’s types available are defined by the platform:
   `Trigger types <../alert/alert.html#InternalTriggerTypes>`__.
-  The possible values ​​for the alert type is also defined by the
   platform and are: INTERNAL, EXTERNAL.

Response data
-------------

As commented before, this action, in addition to the `HTTP status
code <../../general_model.html#reply>`__, returns the list of alerts for
which entity_id has at least read permission.

+--------+-----------------------+----------+
| Key    | Description           | Optional |
+========+=======================+==========+
| alerts | Alerts list (*alert*) | Not      |
+--------+-----------------------+----------+

Every alert element has the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| id                    | Alert ID              | No                    |
+-----------------------+-----------------------+-----------------------+
| name                  | Alert name            | Yes                   |
+-----------------------+-----------------------+-----------------------+
| description           | Alert description     | Yes                   |
+-----------------------+-----------------------+-----------------------+
| entity                | Related entity        | No                    |
+-----------------------+-----------------------+-----------------------+
| type                  | Alert type            | No                    |
+-----------------------+-----------------------+-----------------------+
| trigger               | Trigger type          | No, but only returned |
|                       |                       | for internal alerts   |
+-----------------------+-----------------------+-----------------------+
| expression            | Expression to         | No, but only returned |
|                       | evaluate with the     | for internal alerts   |
|                       | trigger               |                       |
+-----------------------+-----------------------+-----------------------+
| component             | Component identifier  | No, but only returned |
|                       | to which the sensor   | for internal alerts   |
|                       | belongs               |                       |
+-----------------------+-----------------------+-----------------------+
| sensor                | Sensor identifier to  | No, but only returned |
|                       | which the alert       | for internal alerts   |
|                       | applies               |                       |
+-----------------------+-----------------------+-----------------------+

Examples
--------

Request to retrieve all the authorized alerts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following request shows an example to retrieve all the authorized
alerts for rec entity:

::

    http://<your_api_server.com>/catalog/alert/rec

and the response will be:

.. code:: json

   {
     "alerts" : [
       {
         "id" : "REC_ALERT_001",
         "name" : "REC_ALERT_001",
         "description" : "Custom alert to monitorize that maximum daily values for sensor REC_001 ranged from 60 and 80",
         "entity" : "SAMCLA",
         "type" : "EXTERNAL"
       },
       {
         "id" : "REC_ALERT_002",
         "name" : "REC_ALERT_002",
         "description" : "Internal alert to check if S00020114-0 value is greater than 45",
         "entity" : "SAMCLA",
         "type" : "INTERNAL",
         "trigger" : "GT",
         "expression" : "45",
         "component" : "S00020114",
         "sensor" : "S00020114-0"
       }
     ]
   }

Request to retrieve all the authorized alerts filtered by type and trigger
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following request shows an example to retrieve all internal alerts
for rec entity with trigger type equal to GT.

::

    http://<your_api_server.com>/catalog/alert/rec?type=INTERNAL&trigger=GT

and the response will be:

::

   {"alerts":[
      {
         "id" : "REC_ALERT_002",
         "name" : "REC_ALERT_002",
         "description" : "Internal alert to check if S00020114-0 value is greater than 45",
         "entity" : "SAMCLA",
         "type" : "INTERNAL",
         "trigger" : "GT",
         "expression" : "45",
         "component" : "S00020114",
         "sensor" : "S00020114-0"
       } 
   ]}
