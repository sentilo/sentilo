Publish Alarm
=============

Description
-----------

This action allows you to publish an alarm related with an alert. Once
the system receives the alarm, persists it and sends the notification to
all who are subscribed to alarms alert.

::

    http://<your_api_server.com>/alarm/<alert_id> 

+----------------+--------------------------------+
| **Formats**    | json                           |
+----------------+--------------------------------+
| **Method**     | PUT                            |
+----------------+--------------------------------+
| **Permission** | Writing                        |
+----------------+--------------------------------+
| **Return**     | No additional data is returned |
+----------------+--------------------------------+

Parameters
----------

Each alarm will have its own associated information structure defined in
the generic format (JSON).

The platform only persists and transfers the information to recipients
without interpreting its contents.

+---------+-------------+----------+
| Key     | Description | Optional |
+=========+=============+==========+
| message | Free field  | Not      |
+---------+-------------+----------+

Response data
-------------

This actions does not return additional data beyond the `HTTP status
code <../../general_model.html#reply>`__ associated with each request to
the platform.

Examples
--------

Post a new alarm associated with an alert
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following example shows how to send a request to the platform in
order to publish a new alarm associated to an alert with identifier 43:

::

    http://<your_api_server.com>/alarm/43 

and like body message:

::

   {"message":"Threshold limit exceeded: 32"} 

**Please note the following:**\\

-  (% style=“font-size: 16px; background-color: rgb(245, 245, 245);”
   %)If the alert is in offline state, the server rejects the
   publication.
