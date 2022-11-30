Administration console
----------------------

The administration console is composed of several CRUDs used to maintain
all the entities of the Catalog such as providers, components, sensors,
users, … Only registered users can access it, so you must be logged
before starting to manage the Catalog (the login access is located at
the top right menu bar). Remember that, by default, the admin user has
admin/1234 as access credentials.

All admin pages follow the same structure and layout for ease of use and
to facilitate future maintenance. Therefore, below there is only a brief
description of each admin page rather than to repeat the same things
over and over in every section. In these sections will focus only on the
particularities of each one.

When you select any option of the menu admin, the first page that you
will see will be a list with the resources of this type already
registered on the Catalog. These lists are very intuitive and extremely
easy-to-use: you could filter, page and order it. You could delete an
existing resource selecting the corresponding checkbox and clicking the
*Delete selected* button; you could add new resources selecting the
corresponding button and you could edit anyone clicking over the
corresponding row.

|ComponentsTypes.png|

When you select to add a new resource, a traditional form page is
displayed. Here, you must have filled in the mandatory fields before
clicking the *Save* button. If some mandatory field is not filed in or
it have a no valid value, the page shows you information about what is
wrong:

|new_provider_2.png|

Otherwise, the resource will be registered into the Catalog and you will
be redirect to the list page (at the top right corner you will see a
confirmation message that the resource have been successfully created):

|ComponentsTypes_create.png|

The same applies when you try to delete a resource, but with the
peculiarity that the browser will always ask for your confirmation
before deleting it:

|ComponentsTypes_delete.png|

If the resource has been successfully removed, the list is reloaded and a
confirmation message is displayed at the top right corner:

|ComponentsTypes_deleted.png|

Otherwise, you will see an error page with a description about what is
wrong. For example, if you try to delete a component type that is
associated with an existing component the response will be :

|delete_error.png|


Organization
~~~~~~~~~~~~

The organization is the entity that describes the Sentilo instance.

Detail
^^^^^^

By default, this organization is created and its identifier is
**sentilo**.

|Organitzation_detail.png|

We can also edit the organization's name other contact details, except the.

Config params
^^^^^^^^^^^^^

In addition, we can edit the visualization formats and public map
settings, using the **Config params** tab:

|organization_170_001.jpg|

There we can configure the Visual configuration and the Map
configuration.

Visual configuration
''''''''''''''''''''

These params will apply to the entire catalog application visual
customization, and how the user will see the data. Note that time zone &
date format are directly relationated.

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Time zone             | Defines the time zone | You can define hourly |
|                       | of the organization,  | difference or time    |
|                       | and modifies the way  | zone abbreviations:   |
|                       | to display data on    | CET, UTC, +001...     |
|                       | screen, such as dates |                       |
+-----------------------+-----------------------+-----------------------+
| Date format           | Defines the date      | Example: dd/MM/yyyy   |
|                       | format with which the | HH:mm:ss = 30/11/2017 |
|                       | data will be          | 15:34:56              |
|                       | displayed in the      | See all possible      |
|                       | application (lists,   | formats as Java Date  |
|                       | details...)           | Format, at: `Java     |
|                       |                       | Date Format`_         |
+-----------------------+-----------------------+-----------------------+
| Chart values number   | Number of             | It must be a positive |
|                       | observations          | integer number        |
|                       | displayed on chart    | greater or equals to  |
|                       |                       | 10. If blank, it will |
|                       |                       | be a default value of |
|                       |                       | 10.                   |
|                       |                       | This value will be    |
|                       |                       | overwritten by        |
|                       |                       | sensor's              |
|                       |                       | configuration one.    |
+-----------------------+-----------------------+-----------------------+

.. _Java Date Format: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html

Map configuration
'''''''''''''''''

These params configure the universal map visualization.

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Zoom level            | Zoom level of the     | Default value is 14.  |
|                       | universal map         | And you can define a  |
|                       |                       | value between 1 and   |
|                       |                       | 20.                   |
|                       |                       | See possible values   |
|                       |                       | in:                   |
|                       |                       | https://developers.go |
|                       |                       | ogle.com/maps/documen |
|                       |                       | tation/static-maps/in |
|                       |                       | tro#Zoomlevels        |
+-----------------------+-----------------------+-----------------------+
| Latitude / Longitude  | Defines the map       |                       |
|                       | center in latitude &  |                       |
|                       | longitude values      |                       |
|                       | format                |                       |
+-----------------------+-----------------------+-----------------------+
| Map background color  | Define the background | Possible values       |
|                       | color of the map      | applies with the      |
|                       |                       | colorpicker, or input |
|                       |                       | a valid css / html    |
|                       |                       | color value           |
+-----------------------+-----------------------+-----------------------+

For example, set the map background color to #ffc900:

|organization_170_002.jpg|

will result in:

|Changing_map_color.png|


Users
~~~~~

The "Users" section is for managing users of the catalog application. It’s possible to
A catalog user can be one of these three roles:

-  **Admin**: role for administration purposes.
-  **Platform**: platform role for internal use.
-  **User**: visualisation role, they could access to the administration
   console and read all the data, but they haven’t permission for
   changing anything, except own contact details and password.

|users_170_001.jpg|

.. note::

   A non-admin account will only see a menu "User" with his/her own details. A User role cannot see list other users.

**User properties editable by administrator**

+-----------------------+-----------------------+-----------------------+
| Id                    | Name                  | Description           |
+=======================+=======================+=======================+
| Id                    | User identifier       | After its creation    |
|                       |                       | can't be modified     |
+-----------------------+-----------------------+-----------------------+
| Password              | Password              | Admin user can        |
|                       |                       | rewrite user's        |
|                       |                       | password              |
+-----------------------+-----------------------+-----------------------+
| Repeat                | Password check        |                       |
+-----------------------+-----------------------+-----------------------+
| Name                  | User name             |                       |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           |                       |
+-----------------------+-----------------------+-----------------------+
| Creation date         | Creation date         | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Update date           | Last update date      | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| E-Mail                | User e-mail           |                       |
+-----------------------+-----------------------+-----------------------+
| Active                | Checkbox for removing |                       |
|                       | access                |                       |
+-----------------------+-----------------------+-----------------------+
| Role                  | Related role          | Value list            |
+-----------------------+-----------------------+-----------------------+


.. _users-edit-a-user:

Edit a User
^^^^^^^^^^^

|users_170_002.jpg|


.. note::

   A password has to have 8-10 characters, and have at least 1 uppercase, 1 lowercase and 1 digit.



Applications
~~~~~~~~~~~~

Applications are the data clients of the Sentilo platform and, by
default, if you have loaded the default data, you will see two
applications registered into the Catalog:

-  **sentilo-catalog**: it is a internal application, used by the
   catalog to make calls to the API REST and therefore MUST NOT be
   removed.

-  **testApp**: as the name suggest, this application is used for
   testing the platform status.

.. _applications-list:

List
^^^^

Access the Application list. This is the main Application page. From
here you’ll can access to the desired application to show its details by
click on it.

|applications_170_000.jpg|

You’ll be able to list, filter, show application details, create (*New
application* button) and delete selected applications (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

.. _applications-details-tab:

Details tab
^^^^^^^^^^^

The detail page is structured into three tabs:

|applications_170_001.jpg|

where:

-  the *Details* tab contains the main properties of the application
   (described below).
-  the *Permissions*\ tab allows to manage the permissions for other
   entities (applications or providers)
-  the *Active subscriptions* tab displays a list with all the active
   subscriptions for the current application.

The main properties of the *Details* tab are the following:

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Id                    | Application           | Mandatory. After its  |
|                       | Identifier            | creation it can't be  |
|                       |                       | modified. It is the   |
|                       |                       | identifier used in    |
|                       |                       | the API calls.        |
+-----------------------+-----------------------+-----------------------+
| Name                  | Display name          | If not filled in by   |
|                       |                       | the user, its default |
|                       |                       | value will be the     |
|                       |                       | *Id*.                 |
+-----------------------+-----------------------+-----------------------+
| Token                 | Access key            | Automatically         |
|                       |                       | generated by the      |
|                       |                       | system when           |
|                       |                       | application is        |
|                       |                       | created. It is        |
|                       |                       | the\ *identity_key*   |
|                       |                       | value used in the API |
|                       |                       | calls.                |
|                       |                       | *NOTE: only users     |
|                       |                       | with ADMIN role will  |
|                       |                       | show the entire token |
|                       |                       | chain, other user     |
|                       |                       | roles only will see   |
|                       |                       | obfuscated text at    |
|                       |                       | this place (see       |
|                       |                       | below)*               |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           | Optional. The         |
|                       |                       | application           |
|                       |                       | description text.     |
+-----------------------+-----------------------+-----------------------+
| HTTPS API REST        | Application accepts   | The Sentilo Server    |
|                       | data over HTTPS       | itself does not       |
|                       |                       | support SSL at the    |
|                       |                       | moment, however you   |
|                       |                       | can put a reverse     |
|                       |                       | proxy such as Nginx   |
|                       |                       | in front of the       |
|                       |                       | Sentilo Server. If    |
|                       |                       | this option is        |
|                       |                       | checked, the Sentilo  |
|                       |                       | Server expects the    |
|                       |                       | standard header       |
|                       |                       | ::                    |
|                       |                       |                       |
|                       |                       |    X-Forwarded-Proto  |
|                       |                       |                       |
|                       |                       | Please note that when |
|                       |                       | configuring Nginx,    |
|                       |                       | you should also use   |
|                       |                       | the parameter         |
|                       |                       | ::                    |
|                       |                       |                       |
|                       |                       |    underscores_in_hea |
|                       |                       |    ders on;           |
|                       |                       |                       |
|                       |                       | so Nginx would        |
|                       |                       | forward sentilo       |
|                       |                       | headers to the        |
|                       |                       | Sentilo Server.       |
+-----------------------+-----------------------+-----------------------+
| Contact email         | Email address of the  | Mandatory.            |
|                       | person responsible    |                       |
|                       | for the application   |                       |
+-----------------------+-----------------------+-----------------------+

How users that has not ADMIN role see the detail section:

|applications_170_002.jpg|

.. _applications-permissions-tab:

Permissions tab
^^^^^^^^^^^^^^^

As commented before, the *Permissions* tab allows you to define and
manage the authorization privileges that are granted to an application
(such privileges are named *permissions*) which are required for access
to the data from other entities.

There are 3 possibles permissions:

-  *Read*: Only allows to read the data but not modify it (e.g. cannot
   publish orders to sensors/actuators).
-  *Read-Write*: allows to read and write data over the resources of an
   entity, but not administer them (e.g.. cannot create new sensors for
   a provider)
-  *Administration*: full control over an entity and its resources.

By default, **the application sentilo-catalog has granted the
Administration permission over all entities registered into Catalog**
and, as you would expect, an application has full control over itself .

For example, at the following case where the permissions of the
application *testApp* are displayed:

|applications_170_003.jpg|

We will see the following:

-  The application *testApp* could administer the entity *testApp*
   (obviously!)
-  The application *testApp* could read any data from the entity
   *testApp_provider*.

.. _applications-active-subscriptions-tab:

Active subscriptions tab
^^^^^^^^^^^^^^^^^^^^^^^^

This tab allows you to inspect the subscriptions that an application has
registered on the platform (remember that subscriptions are [created
with the API
REST](./api_docs/services/subscription/subscription.html)),
as shown in the following picture:

|application_subscriptionsl.png|

Providers
~~~~~~~~~

In Sentilo, providers are those who send data, i.e. those who publish
the data (in contrast to applications, which consume the data). If you
have loaded the default data, you will see one default provider
registered into the Catalog:

-  **testApp_provider**: as the name suggests, this provider is used for
   checking platform status.

One singularity of the providers list is the *Delete* action: **if you
remove a provider, not only the provider will be deleted from the
backend, but also all its related resources** such as components,
sensors, alerts … and any data published by its sensors, **so be very
careful with this command**.

|providers_170_000.jpg|

.. _providers-list:

List
^^^^

Access the Providers list. This is the main Provider page. From here
you’ll can access to the desired provider to show its details by click
on it.

|providers_170_0000.jpg|


You’ll be able to list, filter, show provider details, create (*New
provider* button) and delete selected providers (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

::

   Use the button panel at the bottom right to navigate through the list (first page, previous page, page number, next page and last page, respectively).

.. _providers-details-tab:

Details tab
^^^^^^^^^^^

The detail page of a provider is structured into five tabs:

|providers_170_001.jpg|

where

-  The *Details* tab contains the main properties of the provider
   (described below).
-  The *Sensors/Actuators* tab displays a list with all sensors owned by
   the current provider (i.e. associated with this provider).
-  The *Components* tab displays a list with all components owned by the
   current provider.
-  The *Active subscriptions* tab displays a list with all the active
   subscriptions for the current provider.
-  The *Documentation* In this tab you can upload any files relevant to
   provider, such as a maintenance guide, etc.

The main properties of the *Details* tab are the following:

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Identifier            | Provider identifier   | Mandatory. After its  |
|                       |                       | creation can't be     |
|                       |                       | modified. It is the   |
|                       |                       | identifier  used in   |
|                       |                       | the API calls.        |
+-----------------------+-----------------------+-----------------------+
| Name                  | Display name          | If not filled in by   |
|                       |                       | the user, its default |
|                       |                       | value will be the     |
|                       |                       | *Id*.                 |
+-----------------------+-----------------------+-----------------------+
| Authorization Token   | Access key            | Automatically         |
|                       |                       | generated by the      |
|                       |                       | system when           |
|                       |                       | application is        |
|                       |                       | created. It is        |
|                       |                       | the\ * identity_key*  |
|                       |                       | value used in the API |
|                       |                       | calls.                |
|                       |                       | *NOTE: only users     |
|                       |                       | with ADMIN role will  |
|                       |                       | show the entire token |
|                       |                       | chain, other user     |
|                       |                       | roles only will see   |
|                       |                       | obfuscated text at    |
|                       |                       | this place (see       |
|                       |                       | below)*               |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           | Optional. The         |
|                       |                       | provider description  |
|                       |                       | text.                 |
+-----------------------+-----------------------+-----------------------+
| HTTPS API REST        | Provider sends data   | The Sentilo Server    |
|                       | over HTTPS            | itself does not       |
|                       |                       | support SSL at the    |
|                       |                       | moment, however you   |
|                       |                       | can put a reverse     |
|                       |                       | proxy such as Nginx   |
|                       |                       | in front of the       |
|                       |                       | Sentilo Server. If    |
|                       |                       | this option is        |
|                       |                       | checked, the Sentilo  |
|                       |                       | Server expects the    |
|                       |                       | standard header       |
|                       |                       | ::                    |
|                       |                       |                       |
|                       |                       |    X-Forwarded-Proto  |
|                       |                       |                       |
|                       |                       | Please note that when |
|                       |                       | configuring Nginx,    |
|                       |                       | you should also use   |
|                       |                       | the parameter         |
|                       |                       |                       |
|                       |                       | ::                    |
|                       |                       |                       |
|                       |                       |    underscores_in_hea |
|                       |                       |    ders on;           |
|                       |                       |                       |
|                       |                       | so Nginx would        |
|                       |                       | forward sentilo       |
|                       |                       | headers to the        |
|                       |                       | Sentilo Server.       |
+-----------------------+-----------------------+-----------------------+
| Contact name          | Name of the person    | Mandatory             |
|                       | responsible for the   |                       |
|                       | provider              |                       |
+-----------------------+-----------------------+-----------------------+
| Contact email         | Email address of the  | Mandatory.            |
|                       | person responsible    |                       |
|                       | for the application   |                       |
+-----------------------+-----------------------+-----------------------+

How users that has not ADMIN role see the detail section:

|providers_170_002.jpg|

.. _providers-sensors-actuators-tab:

Sensors/Actuators tab
^^^^^^^^^^^^^^^^^^^^^

As mentioned before, this tab displays a list with all sensors
associated with the current provider, as shown in the picture below
where the sensors of the provider CINERGIA are listed:

|providers_170_003.jpg|

You could filter, page and order the list but you cannot access to the
sensor detail: it must be done from the sensor list administration.

.. _providers-components-tab:

Components tab
^^^^^^^^^^^^^^

As explained early, this list is very similar to the previous one but
with components.

.. _providers-active-subscriptions-tab:

Active subscriptions tab
^^^^^^^^^^^^^^^^^^^^^^^^

The meaning of this tab is the same as described for the applications.

.. _providers-documentation-tab:

Documentation tab
^^^^^^^^^^^^^^^^^

In this tab you can upload any files relevant to provider (up to 4MB
each). The documents in total should not surpass ~16MB, which the `limit
of MongoDb <https://docs.mongodb.com/manual/reference/limits>`__.

Components
~~~~~~~~~~

Within the context of Sentilo, components have a special meaning: they
are not linked to the API REST (except for the
`catalog <./api_docs/services/catalog/catalog.html>`__ service), i.e.,
components are not required to publish or read data. We use components
in Catalog to group together sensors sharing a set of properties, such
as location, provider, power or connectivity.

You could think of them as physical devices with a set of sensors, like
a weather station or a microcontroller, with multiple sensors connected.
But not necessarily a component needs to have sensors physically
connected to it. A gateway could also be modeled as a component: you
could have a wireless sensor network
(`WSN <http://en.wikipedia.org/wiki/Wireless_sensor_network>`__) where
each sensor sends data to a gateway and then it sends data to Sentilo
using its Ethernet/WiFi/.. connection . In this case, the gateway will
also be a *component*. And finally, if you have a sensor that connects to
Sentilo directly then you will have a component with only one sensor.

In short: in Sentilo, a sensor always need to be related to a
component and providers have its sensors grouped by components, as shown
in the following picture:

|provider-component-sensor.png|

.. _components-list:

List
^^^^

One singularity of the components list page are the two buttons that
allows us to change the visibility of a set of components from *public*
to *private* and vice versa. These buttons apply on the selected rows.

|components_170_001.jpg|


You’ll be able to list, filter, show components details and create (*New
component* button). Like with the providers list, the component list
have a *Delete* button that works as follows:*\* if you remove a
component, not only the component will be deleted from the backend, but
also all its related resources will be deleted*\* such as sensors,
alerts … and any data published by its sensors, **so be very careful
with this command**.

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

::

   Use the button panel at the bottom right to navigate through the list (first page, previous page, page number, next page and last page, respectively).

.. _components-details-tab:

Details tab
^^^^^^^^^^^

The detail page of a component is structured into five tabs:

|components_170_002.jpg|

where:

-  The *Details* tab displays the main properties of the component.
-  The *Technical details* tab displays several categorized properties
   of the component.
-  The *Additional information* tab displays custom properties of the
   component which are not predefined by Sentilo. See the parameter
   `additionalInfo <./api_docs/services/catalog/create_sensors.html#parameters>`__
   of the API docs
-  The *Related components* tab shows other components linked with the
   current component .
-  The *Sensors/Actuators* tab shows the sensor element located in the
   current component.

The main properties of the *Details* tab are the following:

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Name                  | Display name          | Mandatory. After its  |
|                       |                       | creation can't be     |
|                       |                       | modified. It is the   |
|                       |                       | identifier  used in   |
|                       |                       | the API calls.        |
+-----------------------+-----------------------+-----------------------+
| Type                  | Component type.       | Mandatory. Select     |
|                       |                       | from a list of        |
|                       |                       | available types.      |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           | Optional. The         |
|                       |                       | component description |
|                       |                       | text.                 |
+-----------------------+-----------------------+-----------------------+
| Provider              | Component owner       | Mandatory.            |
+-----------------------+-----------------------+-----------------------+
| Photo                 | URL of the component  | It could be defined   |
|                       | photography           | for each component or |
|                       |                       | it will be inherited  |
|                       |                       | using the defined one |
|                       |                       | for the component     |
|                       |                       | type.                 |
+-----------------------+-----------------------+-----------------------+
| Access type           | Checkbox to set the   |                       |
|                       | component visibility  |                       |
|                       | as public or private  |                       |
|                       | in the viewer         |                       |
+-----------------------+-----------------------+-----------------------+
| Creation date         | Creation date         | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Update date           | Last update date      | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Tags                  | Related custom tags   | Are displayed at the  |
|                       | of the component      | public page           |
+-----------------------+-----------------------+-----------------------+
| Static or Mobile      | To mark the component | If the component is   |
|                       | as static or mobile   | static then location  |
|                       |                       | is mandatory          |
+-----------------------+-----------------------+-----------------------+
| Address               | Address where the     | The address,          |
|                       | component is located  | longitude and         |
|                       |                       | latitude fields work  |
|                       |                       | together with the     |
|                       |                       | location list field.  |
|                       |                       | It's possible to use  |
|                       |                       | the map to set the    |
|                       |                       | points adding new     |
|                       |                       | locations.            |
+-----------------------+-----------------------+-----------------------+
| Latitude              | Latitude in decimal   |                       |
|                       | format                |                       |
+-----------------------+-----------------------+-----------------------+
| Longitude             | Longitude in decimal  |                       |
|                       | format                |                       |
+-----------------------+-----------------------+-----------------------+
| Locations List        | Location/s of the     | You can configure the |
|                       | component             | component as a POI, a |
|                       |                       | polyline or a polygon |
|                       |                       | (*future feature*)    |
|                       |                       | depending the         |
|                       |                       | location composition. |
+-----------------------+-----------------------+-----------------------+

.. _components-technical-details-tab:

Technical details tab
^^^^^^^^^^^^^^^^^^^^^

As noted above, this tab displays a set of properties related to the
technical details of the component such as manufacturer, serial number,
….

|comp_tech_details.png|

where:

+-----------------------+-----------------------+-----------------------+
| Property              | Description           | Comments              |
+=======================+=======================+=======================+
| Producer              | Manufacturer          |                       |
+-----------------------+-----------------------+-----------------------+
| Model                 | Component model       |                       |
+-----------------------+-----------------------+-----------------------+
| Serial number         | Serial number         |                       |
+-----------------------+-----------------------+-----------------------+
| MAC                   | Mac address of the    |                       |
|                       | device                |                       |
+-----------------------+-----------------------+-----------------------+
| Power type            | Energy type used by   | Select from a list of |
|                       | the device            | available values (see |
|                       |                       | the API for details)  |
+-----------------------+-----------------------+-----------------------+
| Connectivity type     | Connection type used  | Select from a list of |
|                       | by the device         | available values (see |
|                       |                       | the API for details)  |
+-----------------------+-----------------------+-----------------------+

.. _components-additional-information-tab:

Additional information tab
^^^^^^^^^^^^^^^^^^^^^^^^^^

This tab displays the set of additional properties related to the
component See the parameter `additionalInfo <./api_docs/services/catalog/create_sensors.html#parameters>`__
of the API docs.

These fields are not categorized, i.e., here you could stored any device
information which will be of interest.

For each property, it will be displayed as a *label-value* entry where
the property’s key will be the label and the property’s value will be
the value, as shown in the following picture:

|comp_add_info.png|

where the following map, stored on the backend, has been rendered
*{“Comarca”:“Alt Empordà”,“Terme municipal”:“COLERA”,“Provincia”:“Girona”}*

.. _components-sensors-actuators-tab:

Sensors/actuators tab
^^^^^^^^^^^^^^^^^^^^^

The meaning of this tab is the same as
for the providers, but restricted to the current component.

Sensors
~~~~~~~

These section is used for creating, updating or deleting sensors or
actuators. Usually these elements are created by the provider
autonomously using the API.

The sensors list page follows the same structure as described for
components (you could change the public/private visibility or delete
sensors massively through the list).

.. _sensors-list:

List
^^^^

It is possible to full-text search the list in the “Filter” box. The
filter works for all filter attributes except the creation date. The
Filter field is case-sensitive. Only search by the substate’s code is
possible at the moment.

|sensors_170_000.jpg|

You’ll be able to list, filter, show sensors details, and create (*New
application* button) and delete selected sensors (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

.. _sensors-details-tab:

Details Tab
^^^^^^^^^^^

The detail page of a sensor is structured into four tabs:

|sensor_detail.png|

where

-  The *Details* tab displays the main properties of the sensor.
-  The *Technical details* tab displays several categorized properties
   of the sensor.
-  The *Additional information* tab displays the custom properties of
   the sensor.
-  The *Latest data* tab shows the latest observations received from
   the sensor.

The main properties of the *Details* tab are the following:

+-----------------------+-----------------------+-----------------------------+
| Property              | Description           | Comments                    |
+=======================+=======================+=============================+
| Sensor / Actuator     | Name of the           | Mandatory. After its        |
|                       | sensor/actuator.      | creation can't be           |
|                       |                       | modified. It is the         |
|                       |                       | identifier used in          |
|                       |                       | the API calls.              |
+-----------------------+-----------------------+-----------------------------+
| Provider              | Sensor provider owner | Mandatory                   |
+-----------------------+-----------------------+-----------------------------+
| Description           | Description           |                             |
+-----------------------+-----------------------+-----------------------------+
| Component             | Component to which    | Mandatory                   |
|                       | the sensor belongs    |                             |
+-----------------------+-----------------------+-----------------------------+
| Access type           | Checkbox to set the   |                             |
|                       | sensor visibility to  |                             |
|                       | public or private     |                             |
+-----------------------+-----------------------+-----------------------------+
| Creation date         | Creation date         | Automatically               |
|                       |                       | generated                   |
+-----------------------+-----------------------+-----------------------------+
| Update date           | Last update date      | Automatically               |
|                       |                       | generated                   |
+-----------------------+-----------------------+-----------------------------+
| Type                  | Sensor type           | Mandatory. Select           |
|                       |                       | from a list of              |
|                       |                       | available types             |
+-----------------------+-----------------------+-----------------------------+
| Data type             | Type of data          | Mandatory. Possible         |
|                       | published by the      | values are:                 |
|                       | sensor                |                             |
|                       |                       | -  Audio Link               |
|                       |                       | -  Boolean                  |
|                       |                       | -  File link                |
|                       |                       | -  Image link               |
|                       |                       | -  JSON                     |
|                       |                       | -  Link                     |
|                       |                       | -  Numerical                |
|                       |                       | -  Text                     |
|                       |                       | -  Video Link               |
+-----------------------+-----------------------+-----------------------------+
| Unit                  | Measurement unit      |                             |
+-----------------------+-----------------------+-----------------------------+
| Time zone             | Time zone for the     |                             |
|                       | data sent by the      |                             |
|                       | sensor                |                             |
+-----------------------+-----------------------+-----------------------------+
| Tags                  | Related custom tags   |                             |
|                       | of the sensor         |                             |
+-----------------------+-----------------------+-----------------------------+
| State                 | State of the sensor   | Possible values:            |
|                       |                       | online \| offline. If       |
|                       |                       | the sensor is               |
|                       |                       | configured as offline       |
|                       |                       | the API will reject         |
|                       |                       | any data publication,       |
|                       |                       | the alerts will be          |
|                       |                       | disabled and the            |
|                       |                       | sensor won't be             |
|                       |                       | visible in the map.         |
|                       |                       | Likewise, offline           |
|                       |                       | sensors are excluded        |
|                       |                       | from the /catalog GET       |
|                       |                       | request. Default            |
|                       |                       | value is online.            |
+-----------------------+-----------------------+-----------------------------+
| Substate              | Substate of the       | The list of possible        |
|                       | sensor                | values that have            |
|                       |                       | informational purpose       |
|                       |                       | and are specific for        |
|                       |                       | every deployment. You       |
|                       |                       | can customize the           |
|                       |                       | list of possible            |
|                       |                       | substate values             |
|                       |                       | editing the contents        |
|                       |                       | of table                    |
|                       |                       | sensorSubstate in           |
|                       |                       | mongoDB. No default         |
|                       |                       | value.                      |
+-----------------------+-----------------------+-----------------------------+
| TTL (min)             | Time of expiration of | This value can be           |
|                       | sensor's data in      | configured only             |
|                       | minutes               | from the catalog,           |
|                       |                       | Only admin should           |
|                       |                       | control this value.         |
|                       |                       | The default value is        |
|                       |                       | redis.expire.data.seconds   |
|                       |                       | from the platform server    |
|                       |                       | jedis-config.properties     |
+-----------------------+-----------------------+-----------------------------+

.. _sensors-technical-details-tab:

Technical details tab
^^^^^^^^^^^^^^^^^^^^^

As noted above, this tab displays a set of properties related to the
technical details of the sensor ( such as the *manufacturer*, the
*model*, the *serial number* and the *power type* , all of which are
described in the component section) as shown in the following picture:

|sensors_170_001.jpg|

.. _sensors-visual-configuration-tab:

Visual configuration tab
^^^^^^^^^^^^^^^^^^^^^^^^

The only configurable option in this tab is "Chart values number".
This integer indicates how many measures will be show in the observation chart of the sensor.

.. _sensors-additional-information-tab:

Additional information tab
^^^^^^^^^^^^^^^^^^^^^^^^^^

The meaning of this tab is the same as for the `components <#additional-information>`__.

This tab displays the set of additional properties related to the
component See the parameter `additionalInfo <./api_docs/services/catalog/create_sensors.html#parameters>`__
of the API docs.

These fields are not categorized, i.e., here you could stored any device
information which will be of interest.

For each property, it will be displayed as a *label-value* entry where
the property’s key will be the label and the property’s value will be
the value.

.. _sensors-latest-data-tab:

Latest data tab
'''''''''''''''

This tab, as shown in the following picture:

|sensors_170_002.jpg|

displays both the latest observation published by the sensor and a graph
with its last activity.

.. _navigate-the-last-data-chart-2:

Navigate the last data chart

You can navigate along the dates of the graph by using the buttons
located in the lower right corner of it:

|chart_controls.png|

-  **left arrow**: navigate to the past (only if there are older data)
-  **reload data (center button)**: reload last data / reset chart data
-  **righth arrow**: navigate to the future (only if you have navigated
   or gone into the past before)

Number of chart observations at chart


You can change the number of values shown in the graph. To do this,
within the sensor editing tabs, go to **“Visual configuration”**, and
there edit the value of the **“Chart values number”** field

|sensors_170_003.jpg|

You must inform a positive value number. If blank, then default value
shall be applied as that has been configured in the organization visual
configuration.

Showing complex data


If your sensor data type is text, and it contains a complex data in json
format, Sentilo will show it as a prettified value:

|sensors_170_004.jpg|

in this case you will have the possibility to inspect, expand or
contract the json map shown as a value using the navigation buttons:

**Collapse data:** the json map will be collapsed at all

|sensors_170_005.jpg|

**Expand data:** the json map will be expanded at all (default view)

|sensors_170_006.jpg|

**Collapse to level X:** insert a correct value for the X, and click the
button to collapse to the specified level (default level is 0, first
level)

|sensors_170_007.jpg|

Alerts
~~~~~~

Used for managing internal or external Alerts. Usually, external Alerts
are created by a third party autonomously via the API. This third party
could be a provider or application. Internal Alerts can be defined from
the console or using the API. Internal alerts will always be associated
to a provider.

It’s also possible to delete the items massively from the alerts list.

**Properties**

+-----------------------+-----------------------+-----------------------+
| Id                    | Name                  | Description           |
+=======================+=======================+=======================+
| ID                    | Alert identifier      | After its creation    |
|                       |                       | can't be modified     |
+-----------------------+-----------------------+-----------------------+
| Name                  | Display name          |                       |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           |                       |
+-----------------------+-----------------------+-----------------------+
| Active                | Indicates whether the | When a sensor goes    |
|                       | alert is activated or | into the offline      |
|                       | not                   | state, the associated |
|                       |                       | alerts are also       |
|                       |                       | automatically         |
|                       |                       | deactivated.          |
+-----------------------+-----------------------+-----------------------+
| Creation date         | Creation date         | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Update date           | Last update date      | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Type                  | Alert type            | Internal/External     |
+-----------------------+-----------------------+-----------------------+
| Provider              | Related provider      | For external alerts,  |
|                       |                       | a provider which will |
|                       |                       | generate the          |
|                       |                       | associated alarms.    |
|                       |                       | For internal alerts,  |
|                       |                       | the related data      |
|                       |                       | provider.             |
+-----------------------+-----------------------+-----------------------+
| Application           | Related provider      | Only for external     |
|                       |                       | alerts, application   |
|                       |                       | which will generate   |
|                       |                       | the associated alarms |
+-----------------------+-----------------------+-----------------------+
| Component             | Related component     | Only for internal     |
|                       |                       | alerts                |
+-----------------------+-----------------------+-----------------------+
| Sensor                | Related sensor        | Only for internal     |
|                       |                       | alerts                |
+-----------------------+-----------------------+-----------------------+
| Trigger type          | Type of trigger that  | Only for internal     |
|                       | will be applied       | alerts. Value list,   |
|                       |                       | see the API for       |
|                       |                       | details               |
+-----------------------+-----------------------+-----------------------+
| Expression            | Expression to be      | Only for internal     |
|                       | evaluated             | alerts                |
+-----------------------+-----------------------+-----------------------+

.. _alerts-list:

List
^^^^

Access the Alerts list. This is the main Alert page. From here you’ll
can access to the desired alert to show its details by click on it.

|alerts_170_000.jpg|

You’ll be able to list, filter, show alerts details, create (*New alert*
button) and delete selected alerts (select from left checkbox, and apply
by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

Filtering the alerts list
'''''''''''''''''''''''''

It is possible to full-text search the list in the “filter” box. The
field is case-sensitive. That means that you can search for full or
partial text contained in the identifier, type, trigger or status field.
If you want to search for certain trigger type, currently only searching
by trigger type’s code is possible (e.g. a search for “GT” would return
results in the above screen, whereas a search for “GT(40)” wouldn't).

|alert_list.png|

|alert_edit2.png|

Alerts creation rules
~~~~~~~~~~~~~~~~~~~~~

It is possible to bulk-create alerts for a group of sensors. For
example, attach a rain alert rule to all pluviometers of certain
provider.

.. _alerts-creation-rules--list:

List
^^^^

Accessing “Alert creation rules” menu option opens a list of existing
Alert Rules.

|alertsrules_170_000.jpg|

You’ll be able to list, filter, show alert rules details, create (*New
rules* button) and delete selected rules group (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

Create rules
^^^^^^^^^^^^

To create new alerts, use the “New Rules” button.

|alerts_massive_creation.png|

After pressing the “Confirm” button, a modal window will inform on how
many alerts will be created for given combination of provider, component
type and sensor type.

|alerts_massive_creation_confirm.png|

Subsequently, alerts are created, all having the same rule. At the
moment it is not possible to bulk-create alerts without specifying the
provider.

To bulk-delete alerts with associated with a particular rule, just
select the item from the Alert Rule list and press Delete.


Active Subscriptions
~~~~~~~~~~~~~~~~~~~~

Active Subscriptions is a handy section that gives a view of situation of the subscriptions to Sentilo.

The some system subscribes to Sentilo and then is inaccessible for a long time, this creates a unnecessary queue in the system.


|active_subs_190_001.png|


Sensor types
~~~~~~~~~~~~

Used for creating, updating or deleting sensor types. The sensor types
should be defined through the administrator console before adding
elements to the catalog.

It’s possible to delete elements massively through the sensor list.

**Properties**

+---------------+------------------+--------------------------------------+
| Id            | Name             | Description                          |
+===============+==================+======================================+
| Id            | Type identifier  | After its creation can't be modified |
+---------------+------------------+--------------------------------------+
| Name          | Display name     |                                      |
+---------------+------------------+--------------------------------------+
| Description   | Description      |                                      |
+---------------+------------------+--------------------------------------+
| Creation date | Creation date    | Automatically generated              |
+---------------+------------------+--------------------------------------+
| Update date   | Last update date | Automatically generated              |
+---------------+------------------+--------------------------------------+

.. _sensor-types-list:

List
^^^^

Access the main Type of Sensors / Actuators list page, will show you a
complete list of type of sensors.

|sensorstypes_170_001.jpg|

You’ll be able to list, filter, show typologies details, create (*New
typology* button) and delete selected typology (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

New
^^^

Access to create new typology pressing *New typology* button. You must
inform an identifier, name and description (optional) for the new
typology.

|sensorstypes_170_002.jpg|


Component types
~~~~~~~~~~~~~~~

Used for creating, updating or deleting component types. The component
types should be defined through the administrator console before adding
elements to the catalog.

It’s possible to delete elements massively through the component list.

**Properties**

+-----------------------+-----------------------+-----------------------+
| Id                    | Name                  | Description           |
+=======================+=======================+=======================+
| Id                    | Type identifier       | After its creation    |
|                       |                       | can't be modified     |
+-----------------------+-----------------------+-----------------------+
| Name                  | Display name          |                       |
+-----------------------+-----------------------+-----------------------+
| Description           | Description           |                       |
+-----------------------+-----------------------+-----------------------+
| Creation date         | Creation date         | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Update date           | Last update date      | Automatically         |
|                       |                       | generated             |
+-----------------------+-----------------------+-----------------------+
| Photo                 | Related photo         | Generic picture for   |
|                       |                       | the component type,   |
|                       |                       | will be used if there |
|                       |                       | isn't any specified   |
|                       |                       | for the component     |
|                       |                       | itself                |
+-----------------------+-----------------------+-----------------------+
| Icon                  | Related icon          | Value list from the   |
|                       |                       | deployed icon list.   |
|                       |                       | Used in the maps for  |
|                       |                       | representing the      |
|                       |                       | component             |
+-----------------------+-----------------------+-----------------------+

.. _component-types-list:

List
^^^^

Access the main Component’s typology list page, will show you a complete
list of available type of components.

|componenttypes_170_001.jpg|

You’ll be able to list, filter, show typology details, create (*New
application* button) and delete selected typologies (select from left
checkbox, and apply by *Delete selected* button).

Further, you’ll be able to export the list to Excel, by clicking on
*Export to Excel* button. The result file will contain the list columns
and a number of extra ones from internal database use.

Use the button panel at the bottom right to navigate through the list
(first page, previous page, page number, next page and last page,
respectively).

.. _new-1:

New
^^^

Access to create new typology pressing *New typology* button. You must
inform an identifier, name, description (optional), photo (optional) and
icon for the new typology.

|componenttypes_170_002.jpg|

Metrics
~~~~~~~

The new *Metrics* section provides a real-time monitoring of all Sentilo modules, including agents.
This information is useful when you want to rapidly assess the state of the service. However, for
production monitoring, we recommend to also setup some process watcher and alerting system.

Example of the timeline:

|metrics_190_001.png|

Example of dashboard:

|metrics_190_002.png|

.. note::

   Since the metrics are in real-time only, no history is shown unless you leave the Metrics page open for a while.
   The page will show graphics for this time. If you reload the page, the history will be reset.

New internal monitor API runs on different port that the Sentilo API. It's port number is configured in
:literal:`/sentilo-platform/sentilo-platform-server/src/main/resources/properties/config.properties`. The default value is 7081.

..

    monitor.port=7081

The agent `Metrics Monitor Agent </integrations.html#metrics-monitor-agent>`__ gathers these metrics and persists them in Elasticsearch.


.. |ComponentsTypes.png| image:: ../_static/images/catalog_and_maps/ComponentsTypes.png
.. |new_provider_2.png| image:: ../_static/images/catalog_and_maps/new_provider_2.png
.. |ComponentsTypes_create.png| image:: ../_static/images/catalog_and_maps/ComponentsTypes_create.png
.. |ComponentsTypes_delete.png| image:: ../_static/images/catalog_and_maps/ComponentsTypes_delete.png
.. |ComponentsTypes_deleted.png| image:: ../_static/images/catalog_and_maps/ComponentsTypes_deleted.png
.. |delete_error.png| image:: ../_static/images/catalog_and_maps/delete_error.png
.. |Organitzation_detail.png| image:: ../_static/images/catalog_and_maps/Organitzation_detail.png
.. |organization_170_001.jpg| image:: ../_static/images/catalog_and_maps/organization_170_001.jpg
.. |organization_170_002.jpg| image:: ../_static/images/catalog_and_maps/organization_170_002.jpg
.. |Changing_map_color.png| image:: ../_static/images/catalog_and_maps/Changing_map_color.png
.. |applications_170_000.jpg| image:: ../_static/images/catalog_and_maps/applications_170_000.jpg
.. |applications_170_001.jpg| image:: ../_static/images/catalog_and_maps/applications_170_001.jpg
.. |applications_170_002.jpg| image:: ../_static/images/catalog_and_maps/applications_170_002.jpg
.. |applications_170_003.jpg| image:: ../_static/images/catalog_and_maps/applications_170_003.jpg
.. |application_subscriptionsl.png| image:: ../_static/images/catalog_and_maps/application_subscriptionsl.png
.. |providers_170_000.jpg| image:: ../_static/images/catalog_and_maps/providers_170_000.jpg
.. |providers_170_0000.jpg| image:: ../_static/images/catalog_and_maps/providers_170_0000.jpg
.. |providers_170_001.jpg| image:: ../_static/images/catalog_and_maps/providers_170_001.jpg
.. |providers_170_002.jpg| image:: ../_static/images/catalog_and_maps/providers_170_002.jpg
.. |providers_170_003.jpg| image:: ../_static/images/catalog_and_maps/providers_170_003.jpg
.. |provider-component-sensor.png| image:: ../_static/images/catalog_and_maps/provider-component-sensor.png
.. |components_170_001.jpg| image:: ../_static/images/catalog_and_maps/components_170_001.jpg
.. |components_170_002.jpg| image:: ../_static/images/catalog_and_maps/components_170_002.jpg
.. |comp_tech_details.png| image:: ../_static/images/catalog_and_maps/comp_tech_details.png
.. |comp_add_info.png| image:: ../_static/images/catalog_and_maps/comp_add_info.png
.. |sensors_170_000.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_000.jpg
.. |sensor_detail.png| image:: ../_static/images/catalog_and_maps/sensor_detail.png
.. |sensors_170_001.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_001.jpg
.. |sensors_170_002.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_002.jpg
.. |chart_controls.png| image:: ../_static/images/catalog_and_maps/chart_controls.png
.. |sensors_170_003.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_003.jpg
.. |sensors_170_004.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_004.jpg
.. |sensors_170_005.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_005.jpg
.. |sensors_170_006.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_006.jpg
.. |sensors_170_007.jpg| image:: ../_static/images/catalog_and_maps/sensors_170_007.jpg
.. |alerts_170_000.jpg| image:: ../_static/images/catalog_and_maps/alerts_170_000.jpg
.. |alert_list.png| image:: ../_static/images/catalog_and_maps/alert_list.png
.. |alert_edit2.png| image:: ../_static/images/catalog_and_maps/alert_edit2.png
.. |alertsrules_170_000.jpg| image:: ../_static/images/catalog_and_maps/alertsrules_170_000.jpg
.. |alerts_massive_creation.png| image:: ../_static/images/catalog_and_maps/alerts_massive_creation.png
.. |alerts_massive_creation_confirm.png| image:: ../_static/images/catalog_and_maps/alerts_massive_creation_confirm.png
.. |users_170_001.jpg| image:: ../_static/images/catalog_and_maps/users_170_001.jpg
.. |users_170_002.jpg| image:: ../_static/images/catalog_and_maps/users_170_002.jpg
.. |sensorstypes_170_001.jpg| image:: ../_static/images/catalog_and_maps/sensorstypes_170_001.jpg
.. |sensorstypes_170_002.jpg| image:: ../_static/images/catalog_and_maps/sensorstypes_170_002.jpg
.. |componenttypes_170_001.jpg| image:: ../_static/images/catalog_and_maps/componenttypes_170_001.jpg
.. |componenttypes_170_002.jpg| image:: ../_static/images/catalog_and_maps/componenttypes_170_002.jpg
.. |metrics_190_001.png| image:: ../_static/images/catalog_and_maps/metrics_190_001.png
.. |metrics_190_002.png| image:: ../_static/images/catalog_and_maps/metrics_190_002.png
.. |active_subs_190_001.png| image:: ../_static/images/catalog_and_maps/active_subs_190_001.png
