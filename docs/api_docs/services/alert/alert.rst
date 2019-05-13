Alert
=====

.. toctree::
   :hidden:
   :titlesonly:
   :maxdepth: 1
   
   create_alerts
   update_alerts
   retrieve_authorized_alerts
   delete_alerts
   

Description
-----------

The alert service provides methods to record, edit or retrieve alerts
definition.

All requests for this service will have the following format:

::

   http://<your_api_server.com>/catalog/alert/<entity_id>

where entity_id is optional and should be included depending on the
operation. entity_id can be an Application or a Provider.

There are two alert types: **internal** and **external**.

**The internal alerts are related to specific sensors and its logic is
defined using basic math rules or configuring an inactivity time**. They
should be defined through the catalog console or by the API, but only
using the catalog token.

The related alarms are triggered always by the Sentilo platform when the
alert logic occurs.

**The external alerts are defined by third party entities**, which will
be the responsibles of calculating their logic and throw the related
alarms when applies.

For both cases, the Sentilo platform is responsible of publishing the
alarm for all entities subscribed to the related alert.

Actions
-------

The available actions for this service are: 
   - `Adding alerts <./create_alerts.html>`__ 
   - `Update alerts <./update_alerts.html>`__ 
   - `Retrieve list of authorized alerts <./retrieve_authorized_alerts.html>`__ 
   - `Remove alerts <./delete_alerts.html>`__

Internal trigger types
----------------------

The list of trigger types accepted by Sentilo (and their associated
expressions) are:

+-----------------------+-----------------------+-----------------------+
| Id                    | Description           | Expression value      |
+=======================+=======================+=======================+
| **GT**                | Greater than          | Any numerical value   |
|                       | *<expression>*        |                       |
+-----------------------+-----------------------+-----------------------+
| **GTE**               | Greater than or equal | Any numerical value   |
|                       | *<expression>*        |                       |
+-----------------------+-----------------------+-----------------------+
| **LT**                | Less than             | Any numerical value   |
|                       | *<expression>*        |                       |
+-----------------------+-----------------------+-----------------------+
| **LTE**               | Less than or equal    | Any numerical value   |
|                       | *<expression>*        |                       |
+-----------------------+-----------------------+-----------------------+
| **EQ**                | Equal *<expression>*  | Any value             |
+-----------------------+-----------------------+-----------------------+
| **CHANGE**            | Any change            | Not apply here        |
+-----------------------+-----------------------+-----------------------+
| **CHANGE_DELTA**      | Any variation greater | Any numerical value   |
|                       | to delta              | between 0 and 100     |
|                       | *<expression>*        |                       |
+-----------------------+-----------------------+-----------------------+
| **FROZEN**            | No data received in   | Any numerical value   |
|                       | *<expression>*\ minut |                       |
|                       | es                    |                       |
+-----------------------+-----------------------+-----------------------+

The trigger types only apply for the internal alerts.
