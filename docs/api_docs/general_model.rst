General Model
=============

Intro
-----

**Sentilo offers an open source API based on REST interfaces.**

Representational State Transfer (REST) is a style of architecture that
exploits existing technologies and protocols of the World Wide Web
(WWW).

The communication from external elements with Sentilo will be through
HTTP protocol (Hypertext Transfer Protocol).

Here, briefly describes the concepts of REST terminology that Sentilo
will use:

-  **Resources:** Elements of the information system.
-  **Identifiers:** Unique name that identifies a resource within the
   system.
-  **Representations:** Format of the exchanged data.
-  **Operations:** Actions that can be performed on a resource.
-  **Response codes:** Result of the operation.

Resources
---------

Resources, or pieces of information of Sentilo Platform, are:

-  **Sensor**: item of hardware or software with the ability to generate
   an observation(data).
-  **Component**: corresponding to a element of hardware or software,
   with geospatial location (fixed or mobile) who could be composed by 1
   or more Sensors.
-  **Provider**: entity that represents a grup of components and allows
   them to communicate with Sentilo for sending data and receive
   commands.
-  **Client application / Module**: entity that consumes the data
   processed by the platform.

The actions that can be carried out are:

-  **Applications / Modules**

   -  Register on the platform, but always from the administration
      console.
   -  Send orders to providers/sensors (order service).
   -  Receive data from provider/sensors (data service).
   -  Subscribe to system events (subscribe service).

-  **Providers / Sensors**

   -  Register on the platform (catalog service).
   -  Subscribe to system events (subscribe service).
   -  Publish data (data service).

Sensors and components have always a an associated typology.

Identifier
----------

Unique name that identifies a resource in the system.

In the case of Sentilo, it is an **URLs** (Uniform Resource Locator).

The base URL is composed as follows:

::

   protocol://domain:port/service

and consists of the following parts:

-  **communication protocols:** HTTP or HTTPS.
-  **domain:** Platform server API domain (e.g. localhost).
-  **port:** Port defined for communications with the server API
   (e.g. 8081).
-  **service:** catalog, data, order, etc..

Every service has a custom URL format as specified for each services.

Representations
---------------

Data formats that will supports the platform is currently only  **JSON**.

Example data in JSON format:

.. code:: json

   {"observations":[
       {"value":"12.3","timestamp":"17/09/2012T12:34:45"}
   ]}

Operators
---------

The platform operators are the **HTTP protocol methods**.

In general, the operation associated with the operations used by Sentilo
are:

-  **GET**: Request information.
-  **POST**: Send new data.
-  **PUT**: Update existing data.
-  **DELETE**: Erase data.

**The platform discriminates the action you want perform from the method
used and by the service, provider or sensor specified in the URL
invoked.**

Response
-----

The response to a request to the platform is managed through the
response **HTTP status codes.**

+-----------------------+-----------------------+-----------------------+
| Error Code            | HTTP                  | Description           |
+=======================+=======================+=======================+
| 200                   | Success               | Request accepted and  |
|                       |                       | processed correctly   |
+-----------------------+-----------------------+-----------------------+
| 4xx                   | Client Error          | Error in request      |
|                       |                       | (Wrong format,        |
|                       |                       | forbidden mandatory   |
|                       |                       | parameters, ...)      |
+-----------------------+-----------------------+-----------------------+
| 401                   | Unauthorized          | Unauthorized request: |
|                       |                       | empty or invalid      |
|                       |                       | credential            |
+-----------------------+-----------------------+-----------------------+
| 403                   | Forbidden             | Not authorized for    |
|                       |                       | the requested action  |
+-----------------------+-----------------------+-----------------------+
| 404                   | Not Found             | The requested entity  |
|                       |                       | does not exist        |
+-----------------------+-----------------------+-----------------------+
| 429                   | Too Many Requests     | Global quota or       |
|                       |                       | entity quota exceed   |
+-----------------------+-----------------------+-----------------------+
| 5xx                   | Server Error          | Error processing the  |
|                       |                       | request               |
+-----------------------+-----------------------+-----------------------+

In case of error the response body will include a description of the
problem detected, as shown in the following examples:

This payload is returned when no credential is sent:

.. code:: json

   {"code":401,"message":"Invalid credential null"}

This payload is returned when JSON payload could not be read as JSON:

.. code:: json

   {
        "code":400,
        "message":"SIE03-1398350628224  Bad request data: could not read JSON payload. Please review the following error and try again",
        "errorDetails": ["org.sentilo.common.exception.MessageNotReadableException: Unexpected character ('o' (code 111)): ....."]
   }
