Setup
=====

This guide describes how to: **download, configure, compile and install
the last version of Sentilo in your own runtime environment**. Moreover,
it details which are the infrastructure elements necessary for running
Sentilo and how should be their default configuration settings. It’s
assumed you have the skills to configure and install the necessary
software base(Operating System, Maven,JDK, Mongo DB, Redis, etc).

The main topics are:

-  **Prerequisites**: describes the software elements that have to be
   installed before download the code.
-  **Download and build**: explains the steps to obtain the Sentilo
   code, to adapt it and how to build the platform artifacts.
-  **Platform infrastructure**: describes the mandatory infrastructure
   components for running Sentilo and its default configuration
   settings.
-  **Deploy the artifacts**: describes the necessary steps to deploy all
   the Sentilo modules

Prerequisites
-------------

Sentilo uses Maven as a mechanism for building and managing the
dependencies of the platform. In order to build **Sentilo**, it is
necessary to ensure the next set of prerequisites:

-  JDK 1.8.x +
-  Git **(optional)**
-  Maven 3 +
-  Ensure that the the Java SDK and Maven executables are accessible
   using your PATH environment variable.

Download and build code
-----------------------

The Sentilo code must be downloaded from Github. Once downloaded, you
can build it using a script named *buildSentilo.sh* which constructs the
Sentilo artifacts “out-of-the-box”.

Download the source code from Github
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The source code of the project can be obtained from git, cloning the
remote project in a local directory named *sentilo*:

::

   git clone https://github.com/sentilo/sentilo.git sentilo

An alternative method is to download a ZIP file from github repository
and decompress it in a folder named *sentilo*:

https://github.com/sentilo/sentilo/archive/master.zip

In both cases, we will finally have a new directory named *sentilo* with
the source code.

Compiling and build artifacts
-----------------------------

Without changing the default configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want to build Sentilo out-of-the-box (i.e. build all artifacts
that define the Sentilo platform without changing any of the default
settings that are defined), we distribute a script named
*./scripts/buildSentilo.sh* which can be used to build Sentilo from the
command line.

This script compiles the code and build the artifacts from scratch, but
it doesn't deploy them in the execution environments. This process must
be done manually by different reasons, for example:

-  The deployment environment could be distributed in different servers.
   In example, Tomcat server and Pub/Subscribe server.
-  it’s not required to install all the components, like the relational
   database agent.

Changing default settings
~~~~~~~~~~~~~~~~~~~~~~~~~

If you want modify the code before to build it, you should import it
into an Eclipse workspace with maven plug-in installed. Below we explain
how to do it by using the M2E plugin.

-  Open the Eclipse workspace to import the code:

   -  Go to **File> Import> Existing Maven Projects**
   -  Select **./sentilo** as the root directory
   -  Select all projects and import

**Warning**: be sure that JDK 1.8, or later, is correctly configured in
your Eclipse environment.

After modifying the code, to compile and build the artifacts, our
recommendation is to use the above mentioned\* buildSentilo\* script.

Platform infrastructure
-----------------------

Before describing how to install all the Sentilo components, we’re going
to explain how to configure each element of the infrastructure.

Sentilo uses the following infrastructure elements (they are grouped
into two categories):

-  Mandatory

   -  Redis 4.0.11
   -  MongoDB 4.0.1
   -  Tomcat 8.5.32 +

-  Optional

   -  MySQL 5.5.x (Sentilo has been tested on MySQL 5.5.34 but you could
      use your favourite RDBMS) **It is only necessary if you want to
      install the relational agent**
   -  Elasticsearch 5+ **It is only necessary if you want to install
      the activity-monitor agent**.
   -  openTSDB 2.2.0 + **It is only necessary if you want to install the
      historian agent**

You must ensure that you have all these elements installed properly (you
can find information on how to install them in each provider site).

Below we explain the default settings for each Sentilo module.

Default settings
~~~~~~~~~~~~~~~~

Sentilo configuration uses the Spring and Maven profiles to allow its
customization depending on the runtime environment. By default, the
platform comes with a predefined profile named **dev**, which considers
that each of these infrastructure elements are installed on the same
machine and listening in the following ports:

-  Redis: 6379
-  MongoDB: 27017
-  Tomcat: 8080
-  MySQL: 3306
-  Elasticsearch: 9200
-  openTSDB: 4242

All these settings can be found in the subdirectory
:literal:`/src/main/resources/properties` of each platform’s module.

Redis settings
~~~~~~~~~~~~~~

Sentilo default settings consider Redis will be listening on port 6379,
host 127.0.0.1, and with the parameter
`requirepass <http://redis.io/commands/AUTH>`__ enabled and with value
**sentilo**.

If you change this behaviour, you need to modify the following
properties:

.. code:: properties

   jedis.pool.host=127.0.0.1
   jedis.pool.port=6379
   jedis.pool.password=sentilo

which are configured in the following files:

::

   sentilo-platform/sentilo-platform-service/src/main/resources/properties/jedis-config.properties
   sentilo-agent-alert/src/main/resources/properties/jedis-config.properties
   sentilo-agent-relational/src/main/resources/properties/jedis-config.properties
   sentilo-agent-location-updater/src/main/resources/properties/jedis-config.properties

MongoDB settings
~~~~~~~~~~~~~~~~

Sentilo default settings consider MongoDB will be listening on
127.0.0.1:27017, and requires an existing database named *sentilo*,
created before starting the platform, with `authentication
enabled <http://docs.mongodb.org/v4.0/core/access-control/>`__ and with
login credentials preconfigured as sentilo/sentilo (username~:*sentilo*,
password~:\ *sentilo*).

If you change this behaviour, you need to modify following properties:

.. code:: properties

   catalog.mongodb.host=127.0.0.1
   catalog.mongodb.port=27017
   catalog.mongodb.user=sentilo
   catalog.mongodb.password=sentilo

configured in the following files:

::

   sentilo-agent-alert/src/main/resources/properties/catalog-config.properties
   sentilo-catalog-web/src/main/resources/properties/catalog-config.properties

Data load
^^^^^^^^^

Moreover, you need to load on *sentilo* database the basic set of data
needed to run the platform. The data include, among other things:

-  An user **admin**: user for log in into the catalog webapp as
   administrator.
-  An user **sadmin**: user for log in into the catalog webapp with role
   super-admin.
-  A default **sentilo** tenant: used to configure the default viewer
   parameters (center, zoom, … ) from the catalog web app.
-  An entity **sentilo-catalog**: internal app used by the platform to
   synchronize information between its components.
-  An user **platform_user**: internal user used by the platform to
   synchronize information between its components.

To do this, you must load the data defined in the file:

::

   ./scripts/mongodb/init_data.js

For example, in your MongoDB machine, you should execute the following
command from the directory where the file is located:

::

   mongo -u sentilo -p sentilo sentilo init_data.js

.. note::

   The file init_data.js contains
   default passwords and tokens (which are ok for run Sentilo in a
   test environment). In order to avoid compromising your platform, we
   recommend to change them before installing Sentilo in a production
   environment.

If you change default values in the :literal:`/sentilo/scripts/mongodb/init_data.js` file and load them to
MongoDB, you will have to modify the following properties before compiling and building Sentilo. So, following
JS code from *init_data.js* :

.. code:: javascript
   db.application.insert({ "_id" : "sentilo-catalog", "_class" : "org.sentilo.web.catalog.domain.Application", "name" : "sentilo-catalog", "token" : "c956c302086a042dd0426b4e62652273e05a6ce74d0b77f8b5602e0811025066", "description" : "Catalog application", "email" : "sentilo@sentilo.org", "createdAt" : new ISODate(), "authorizedProviders" : [ ] });
   db.user.insert({ "_id" : "platform_user", "_class" : "org.sentilo.web.catalog.domain.User", "password" : "sentilo", "name" : "Platform user", "description" : "PubSub platform user. Do not remove  it!.", "email" : "sentilo@sentilo.org", "createdAt" : new ISODate(), "active" : true, "roles" : [ "PLATFORM" ] });

Corresponds with:

.. code:: properties

   rest.client.identity.key=c956c302086a042dd0426b4e62652273e05a6ce74d0b77f8b5602e0811025066
   catalog.rest.credentials=platform_user:sentilo

, being *rest.client.identity.key* the token of a *sentilo-catalog* application, and *catalog.rest.credentials* value
is a combination of user *platform_user* and it's password.

These properties are in following files:

::

   sentilo-agent-alert/src/main/resources/properties/platform-client-config.properties
   sentilo-catalog-web/src/main/resources/properties/catalog-config.properties
   sentilo-platform/sentilo-platform-service/src/main/resources/properties/integration.properties

Test data load
^^^^^^^^^^^^^^

In order to validate the correct installation of the platform, we could
load a set of test data. These data includes, among other things: sensor
types, component types, apps and providers.

These data is defined in the file:

::

   ./scripts/mongodb/init_test_data.js

and, as pointed above, you should run the following command to load it:

::

   mongo -u sentilo -p sentilo sentilo init_test_data.js

MySQL settings
~~~~~~~~~~~~~~

.. note::

   This software is mandatory only if you want to export the published
   events to a relational database using the Relational Database Agent. Otherwise, you
   can skip this step. Please, check `this <./integrations.html#relational-database-agent>`__ out for
   more info.

Sentilo default settings consider MySQL server will be listening on
127.0.0.1:3306, and requires an existing database named *sentilo*,
created before starting the platform, with authentication enabled and
accessible using credentials *sentilo_user/sentilo_pwd*
(username~:*sentilo_user*, password~:\ *sentilo_pwd*).

If you change this behaviour, you need to modify the following
properties:

.. code:: properties

   sentiloDs.jdbc.driverClassName=com.mysql.jdbc.Driver
   sentiloDs.url=jdbc:mysql://127.0.0.1:3306/sentilo
   sentiloDs.username=sentilo_user
   sentiloDs.password=sentilo_pwd

configured in the file:

::

   sentilo-agent-relational/src/main/resources/properties/relational-config.properties

Creating the tables
^^^^^^^^^^^^^^^^^^^

Once we have MySQL configured, and the database *sentilo* created, the
next step is to create the database tables required to persist
historical platform data.

At the following directory of your Sentilo installation:

::

   sentilo-agent-relational/src/main/resources/bd 

you’ll find the script to create these tables.

Tomcat settings
~~~~~~~~~~~~~~~

Sentilo default settings consider Tomcat will be listening on
127.0.0.1:8080.

If you change this behaviour, you need to modify the following property:

.. code:: properties

   catalog.rest.endpoint=http://127.0.0.1:8080/sentilo-catalog-web/

configured in the following files:

::

   sentilo-platform/sentilo-platform-service/src/main/resources/properties/integration.properties
   sentilo-agent-location-updater/src/main/resources/properties/integration.properties

Your Tomcat should also be started with the user timezone environment
variable set as UTC. To set Timezone in Tomcat, the startup script (e.g.
*catalina.sh* or *setup.sh*) must be modified to include the following
code:

::

   -Duser.timezone=UTC


API server (Subscription/publication) settings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sentilo default settings consider subscription/publication server
(a.k.a. *PubSub* server) will be listening on 127.0.0.1:8081

If you change this behaviour, you need to modify the following
properties:

.. code:: properties

   port=8081
   rest.client.host=http://127.0.0.1:8081

configured in the following files:

::

   sentilo-platform/sentilo-platform-server/src/main/resources/properties/config.properties
   sentilo-catalog-web/src/main/resources/properties/catalog-config.properties

Configuring logs
~~~~~~~~~~~~~~~~

Sentilo uses **slf4j** and **logback** as trace frameworks. The
configuration can be found in **logback.xml** file, located in the
subdirectory **src/main/resources** of sentilo-common module of the
platform.

By default, all platform logs are stored in the directory
**/var/log/sentilo**

Platform installation
---------------------

Once you have downloaded the code and you have modify, compile and built
it, the next step is to deploy Sentilo artifacts. The platform has five
artifacts:

-  Web Application Catalog (is **mandatory**)
-  Server publication and subscription (is **mandatory**)
-  Alarms agent is not strictly mandatory, however you'll need it if you want to provide alerts.
-  Location updater agent is not strictly mandatory, however you'll need it if you want to update locations
   in case you have mobile components.
-  All other agents (are **optional**):


Installing the Web App Catalog
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After build Sentilo, to install the Web App, you just need to deploy the
WAR artifact in your Tomcat server, i.e., copy the WAR artifact into the
*webapps* subdirectory of your Tomcat server.

You will find the WAR artifact at the following subdirectory:

::

   ./sentilo-catalog-web/target/sentilo-catalog-web.war

Installing API server (subscription/publication)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After build Sentilo, to install the API (pub/sub) server, you need to follow
the following steps:

a. Into the directory
   *./sentilo-platform/sentilo-platform-server/target/appassembler*
   you’ll find two subdirectories named **repo** and **bin**:

-  **repo** directory contains all libraries needed to run the process
-  **bin** directory contains the script (*sentilo-server*) needed to
   initialize the process (there are two scripts, one for Linux systems
   and one for Windows)

b. Copy these two directories in the root directory where you want to
   install this component (for example: /opt/sentilo-server).

c. Once copied, for starting the process you just need to run the
   script:

::

     $sentilo-server/bin/sentilo-server

Installing agents
~~~~~~~~~~~~~~~~~

As have been mentioned previously, all agents are optional and you are
free to choose which of them will be deployed, depending on your
specific needs. Agents are internal modules oriented to expand the
platform functionality without having to alter its core. You will find
more information about them in the `Integrations <./integrations.html#agents>`__
section of our documentation.

The *buildSentilo* script builds also all agents. If you decide to install some of them,
you just have to copy the contents of the appassembler directory to the path you want the
agent to be installed.

For example, Alert agent would be installed like this:

a. In the directory *./sentilo-agent-alert/target/appassembler* you’ll
   find two subdirectories named **repo** and **bin**:

-  **repo** directory contains all libraries needed to run the process
-  **bin** directory contains the script (*sentilo-agent-alert-server*)
   needed to initialize the process (there are two scripts, one for
   Linux systems and one for Windows)

b. Copy these two directories in the root directory where you want to
   install this component (for example: /opt/sentilo-agent-alert).

c. Once copied, for starting the process you just need to run the
   following script:

::

     <path-to-agent-alert>/bin/sentilo-agent-alert-server

All other agents follow the exact same directory structure.

.. note::

   The configuration of the agents has to be done before compilation
   and is documented in their `respective page <./integrations.html#agents>`__



Enable multi-tenant instance
----------------------------

In order to enable multi-tenant feature you need to ensure that your
Sentilo version is at least 1.5.0. Otherwise you will have to
`upgrade <https://github.com/sentilo/sentilo/wiki/How-to-upgrade-Sentilo>`__
your Sentilo instance.

Once the above requirement is fulfilled, you only need to do the
following steps:

Modify your Tomcat startup script
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You should modify your Tomcat startup script (e.g
*%TOMCAT_HOME%/bin/catalina.sh* or *%TOMCAT_HOME%/bin/setenv.sh*) to add
a new JVM property:

::

   -Dsentilo.multitenant=true

Once you have added the JVM property, you must restart your Tomcat
server.

Edit the Catalog web.xml file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next step is to edit the Catalog file *web.xml* located at:

::

   sentilo-catalog-web/src/main/webapp/WEB-INF/web.xml

You will find some lines that are commented into this file which are
needed to enable the multi-tenant feature. Therefore you should
uncomment them:

.. code:: xml

   <!-- 
       <filter>
           <filter-name>UrlRewriteFilter</filter-name>
           <filter-class>org.tuckey.web.filters.urlrewrite.UrlRewriteFilter</filter-class>
           <init-param>
               <param-name>logLevel</param-name>
               <param-value>slf4j</param-value>
           </init-param>
       </filter>
       
       
       <filter>
           <filter-name>tenantInterceptorFilter</filter-name>
           <filter-class>org.sentilo.web.catalog.web.TenantInterceptorFilter</filter-class>
       </filter>
   -->

   <!--
       <filter-mapping>
           <filter-name>tenantInterceptorFilter</filter-name>
           <url-pattern>/*</url-pattern>
           <dispatcher>REQUEST</dispatcher>        
       </filter-mapping>
       <filter-mapping>
           <filter-name>UrlRewriteFilter</filter-name>
           <url-pattern>/*</url-pattern>
           <dispatcher>REQUEST</dispatcher>
           <dispatcher>FORWARD</dispatcher>          
       </filter-mapping>
   -->

Once you have uncomment the above lines, you should recompile the
Catalog webapp module and redeploy it into your Tomcat server.

You will find more information about this feature in the
`Multi-Tenant <./multitenant.html>`__ section of our documentation.

Enable anonymous access to REST API
-----------------------------------

By default, anonymous access to REST API is disabled, e.g.
all requests to REST API must be identified with the
`identity_key <./api_docs/security.html>`__ header.

Enabling anonymous access to the REST API means that only
*authorized* data of your Sentilo instance can be accessed.
Access to authorized data is described below.

In order to enable anonymous access you should modify the file
sentilo-platform/sentilo-platform-server/src/main/resources/properties/config.properties:


.. code:: properties

   # Properties to configure the anonymous access to Sentilo
   enableAnonymousAccess=false
   anonymousAppClientId=


If anonymous access is enabled (*enableAnonymousAccess=true*),
then all anonymous requests to REST API are internally considered as is they have
been performed by the application client identified by the *anonymousAppClientId* property
value (this application client should exist into your Sentilo Catalog),
and therefore these requests will have the same data restrictions as the
requests performed by this client application.

What next?
----------

Check the `Quick Start Page <./quickstart.html>`__ or `Platform
Testing <./platform_testing.html>`__ page.
