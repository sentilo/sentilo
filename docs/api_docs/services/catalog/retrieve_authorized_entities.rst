Retrieve providers / sensors list
=================================

Description
-----------

This resource returns a list of providers and sensors for which you have
at least read permission. Sensors that are in the offline state won’t be
listed. In addition, the service provides optional filtering by sensor
type, component type and component name.

::

    http://<your_api_server.com>/catalog

+----------------+-----------------------------------+
| **Format**     | json                              |
+----------------+-----------------------------------+
| **Method**     | GET                               |
+----------------+-----------------------------------+
| **Permission** | Reading                           |
+----------------+-----------------------------------+
| **Return**     | List of providers, with their     |
|                | sensors, on which we has at least |
|                | read permission                   |
+----------------+-----------------------------------+


Parameters
----------

+---------------+-------------------------+----------+
| Key           | Description             | Optional |
+===============+=========================+==========+
| type          | Sensor's type filter    | Yes      |
+---------------+-------------------------+----------+
| component     | Component name filter   | Yes      |
+---------------+-------------------------+----------+
| componentType | Component's type filter | Yes      |
+---------------+-------------------------+----------+


Response data
-------------

As mentioned, this action, in addition to the `HTTP status
code <../../general_model.html#reply>`__, returns the list of providers
for wich we have at least read permission.

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| providers             | Providers list        | Not                   |
|                       | *(provider)* with at  |                       |
|                       | least read permission |                       |
+-----------------------+-----------------------+-----------------------+

Each provider will have the following structure:

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| provider              | Provider ID           | No                    |
+-----------------------+-----------------------+-----------------------+
| permission            | Indicates whether it  | No                    |
|                       | readable (R) or write |                       |
|                       | (W) on the provider   |                       |
+-----------------------+-----------------------+-----------------------+
| sensors               | Provider list of      | No                    |
|                       | sensors (sensor)      |                       |
+-----------------------+-----------------------+-----------------------+

Each list element **(sensor)** will have the following structure.

+-----------------------+-----------------------+-----------------------+
| Key                   | Description           | Optional              |
+=======================+=======================+=======================+
| sensor                | Sensor identifier     | No                    |
+-----------------------+-----------------------+-----------------------+
| description           | sensor description    | Yes                   |
+-----------------------+-----------------------+-----------------------+
| dataType              | Data sensor type:     | No                    |
|                       |                       |                       |
|                       | -  AUIDO_LINK         |                       |
|                       | -  BOOLEAN            |                       |
|                       | -  FILE_LINK          |                       |
|                       | -  IMAGE_LINK         |                       |
|                       | -  JSON               |                       |
|                       | -  LINK               |                       |
|                       | -  NUMBER             |                       |
|                       | -  TEXT               |                       |
|                       | -  VIDEO_LINK         |                       |
|                       |                       |                       |
+-----------------------+-----------------------+-----------------------+
| location              | Location where de     | Yes                   |
|                       | sensor is             |                       |
+-----------------------+-----------------------+-----------------------+
| type                  | Sensor type           | No                    |
+-----------------------+-----------------------+-----------------------+
| unit                  | Unities in the sensor | Yes                   |
|                       | data coming           |                       |
+-----------------------+-----------------------+-----------------------+
| timeZone              | Sensor's timezone     | Yes                   |
+-----------------------+-----------------------+-----------------------+
| publicAccess          | Visualization check   | Yes                   |
|                       | for the public area   |                       |
+-----------------------+-----------------------+-----------------------+
| component             | Component is          | No                    |
|                       | associated the sensor |                       |
+-----------------------+-----------------------+-----------------------+
| componentType         | Component type        | No                    |
+-----------------------+-----------------------+-----------------------+
| componentDesc         | Component description | Yes                   |
+-----------------------+-----------------------+-----------------------+
| componentPublicAccess | Visualization check   | Yes                   |
|                       | for the public area   |                       |
+-----------------------+-----------------------+-----------------------+
| additionalInfo        | Additional params     | Yes                   |
|                       | related to the sensor |                       |
+-----------------------+-----------------------+-----------------------+
| technicalDetails      | Technical params      | Yes                   |
|                       | related to the sensor |                       |
+-----------------------+-----------------------+-----------------------+
| componentTechnicalDet | Technical params      | Yes                   |
| ails                  | related to the        |                       |
|                       | component             |                       |
+-----------------------+-----------------------+-----------------------+


Examples
--------

Request to retrieve all Providers / Sensors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

    http://<your_api_server.com>/catalog

in the response we will receive

.. code:: json

   {
       "providers": [{
           "provider": "A",
           "permission": "WRITE",
           "sensors": [{
               "sensor": "MAR_01_00_SN001_1010",
               "description": "Sound Sensor MODI 001",
               "dataType": "NUMBER",
               "type": "noise",
               "unit": "dBa",
               "state": "online",
               "component": "MAR_01_00_SN001_1010",
               "componentType": "generic",
               "timeZone": "CET"
           }]
       }, {
           "provider": "C",
           "permission": "READ",
           "sensors": [{
               "sensor": "MAR_02_20_PM001_1010",
               "description": "PM10 Sensor IMI 001",
               "dataType": "NUMBER",
               "type": "air_quality_pm10",
               "unit": "ug/m3",
               "state": "online",
               "component": "air_quality",
               "componentType": "generic"
           }, {
               "sensor": "MAR_02_20_PM001_1012",
               "description": "PM10 Sensor IMI 002",
               "dataType": "NUMBER",
               "type": "air_quality_pm10",
               "unit": "ug/m3",
               "state": "online",
               "component": "air_quality",
               "componentType": "generic",
               "additionalInfo": {
                   "supportMail": "support@imi.com"
               },
               "technicalDetails": {
                   "producer": "xxxx",
                   "model": "x-1",
                   "serialNumber": "9999",
                   "energy": "220VAC"
               },
               "componentTechnicalDetails": {
                   "producer": "XXXX",
                   "model": "X-1",
                   "serialNumber": "9999",
                   "macAddress": "00:17:4F:08:5F:61",
                   "energy": "12_24_VDC",
                   "connectivity": "WIFI"
               }
           }]
       }]
   }

Request to recover all the sensors in the catalog filtered by type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The request in this case is very similar to the previous one adding the
type parameter:

::

    http://<your_api_server.com>/catalog?type=air_quality_pm10

In this case as a response we will receive:

.. code:: json

   {"providers":[
       {
        "provider":"C","permission":"READ",
        "sensors":
        [{
          "sensor":"MAR_02_20_PM001_1010",
          "description":"PM10 Sensor IMI 001",
          "dataType":"NUMBER",
          "type":"air_quality_pm10",
          "unit":"ug/m3",
          "component":"air_quality",
          "componentType":"generic"
         },{
          "sensor":"MAR_02_20_PM001_1012",
          "description":"PM10 Sensor IMI 002",
          "dataType":"NUMBER",
          "type":"air_quality_pm10",
          "unit":"ug/m3",
          "component":"air_quality",
          "componentType":"generic",
          "additionalInfo":{"field1":"value1","field2":"value2"}
         }
        ]
       }
   ]}

Other examples
~~~~~~~~~~~~~~

::

   http://<your_api_server.com>/catalog?component=comp_demo&type=air_quality_pm10

::

   http://<your_api_server.com>/catalog?componentType=air_quality&type=air_quality_pm10


.. note::

   Only sensors will "online" state are returned by the API.