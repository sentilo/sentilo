Data
====

.. toctree::
   :hidden:
   :titlesonly:
   :maxdepth: 1
   
   publish_sensor_data
   publish_provider_sensor_data
   delete_sensor_data
   retrieve_sensor_data
   retrieve_provider_sensor_data
   

Description
-----------

The data service allows to read, write or delete the observations of the
registered sensors.

All requests for this service will have the following format:

::

    http://<your_api_server.com>/data/<provider_id>/<sensor_id>

where **<provider_id>** and **<sensor_id>** correspond to the sensor and 
provider identifiers on which we want to perform the requested action.

Actions
-------

The available actions for this service are:

-  `Publish observations of a sensor <./publish_sensor_data.html>`__
-  `Publish observations from sensors of a
   provider <./publish_provider_sensor_data.html>`__
-  `Delete observations <./delete_sensor_data.html>`__
-  `Read observations from a sensor <./retrieve_sensor_data.html>`__
-  `Read observations from sensors of a
   provider <./retrieve_provider_sensor_data.html>`__
