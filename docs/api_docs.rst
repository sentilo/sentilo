API Docs
========

Contents:

.. toctree::
   :maxdepth: 1

   api_docs/general_model
   api_docs/security
   api_docs/rate_limiting
   api_docs/monitor_api
   api_docs/services

The Application Programming Interface (API) of Sentilo defines a set of commands,
functions and protocols that must be followed by who wants to interact with the system externally.

This area defines the Application Programming Interface (API), that any sensor or application must use to interact
with the platform.

The starting capacities of the platform related to its external interface are:

-  Allows to register applications/modules and providers/sensors in the platform (Catalog).
-  Allow to applications/modules and sensors subscribe to services defined in the catalog as well as post events occurring (Publish/Subscribe).
-  Allow you to send information from sensors to applications/modules (Data).
-  Allows to send orders from applications/modules to sensors (Order).