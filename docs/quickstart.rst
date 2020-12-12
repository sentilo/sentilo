Quickstart
==========

Perquisites
-----------

You should have 4 components up & running:
-  Redis Server
-  MongoDB
-  Sentilo API, running at http://127.0.0.1:8081
-  Sentilo Catalog, running at http://127.0.0.1:8080/sentilo-catalog-web

The installation covered by the section  `Sentilo Setup <./setup.html>`__.
( No need to setup any agents or other optional components such as Elasticsearch or OpenTSDB).
Alternatively, you can use our `VM  <./use_a_virtual_machine.html>`__.

Create a Provider, Component and a Sensor
-----------------------------------------

In order to create a publication of sensor data, we have to create first
the Provider, Component and a Sensor.

We’ll do that from the catalog application as superuser, using the
admin/1234 credentials.

A provider is an entity that manages devices (sensors). We’ll have to
create one from the menu “Providers” -> “New Provider”

A component is a device that contains one or more sensors (such as a
Raspberry PI). We’ll have to create one from the menu “Components” ->
“New Component”. Make sure you select the provider created above.

Finally, we’ll have to create a sensor from the menu “Sensors” -> “New
Sensor”. Make sure you select the component created above. Please select
a numeric type of sensor.

Publish an Observation
----------------------

In order to publish an observation, we’ll use Sentilo’s HTTP REST API.
For that you can use the curl program of some more graphical tool such
as `Postman <https://getpostman.com>`__:

::

   curl -X PUT -H "IDENTITY_KEY: <your provider's token>" http://<your sentilo url>/data/<your provider>/<your sensor>/42.0

The server should respond with HTTP status 200.

Read your Observations
----------------------

::

   curl -X GET -H "IDENTITY_KEY: <YOUR_KEY>" http://<your sentilo url>/data/<your provider>/<your sensor>

The response would be similar to:

.. code:: json

   {
     "observations": [
       {
          "value": "42.0",
          "timestamp": "22/11/2016T11:52:28",
          "location": ""
       }
     ]
   }

Also, on the “Latest Data” tab of the sensor’s page in the catalog will
appear your value, in this case, a 42.0.

What next?
----------

Check the `API documentation <./api_docs.html>`__ here.
