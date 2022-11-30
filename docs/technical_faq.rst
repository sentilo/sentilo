Technical FAQ
=============

In which platforms has been Sentilo tested ?
--------------------------------------------

The first deployment for the Barcelona City Council has
the following infrastructure:

-  Four virtual machines, two for the front-ends and another two for the
   back-end
-  All of them use as operating system Ubuntu server LTS 18.04
-  The real time database server(Redis) works with 32 GB of memory and
   36 GB of hard disk
-  The other three servers work with 4 GB of memory and 16 GB of hard
   disk

Another deployment configurations should work properly, always keeping in
mind the expected load by the system. There is also a `virtual
machine <./use_a_virtual_machine.html>`__ ready for use that can be used
for testing purposes.

All known Sentilo instances are deployed on Linux servers, mainly CentoOS 6+
and Ubuntu Server 14.04+.

I successfully published an observation, but I cannot see the data in catalog.
------------------------------------------------------------------------------

Check that the Catalog and Sentilo API Server are in the same timezone,
for example in UTC. Make sure the sentilo-server is executed with the
following VM option:

::

   -Duser.timezone=UTC

Also, make sure that the Tomcat that hosts the Catalog application has
the same option, for example en $JAVA_OPTS variable.

--------------

Google Maps is not showing up in Catalog application
---------------------------------------------

Recently Google changed it policy regarding Maps key. Please go to
https://developers.google.com/maps/documentation/javascript/get-api-key
and create one.

You can define the API key inside the :literal:`sentilo/sentilo-catalog-web/src/main/resources/properties/catalog-config.properties` configuration file:

.. code:: properties

   # Google API key to use Google Maps
   google.api.key=<your key> 

--------------

Remember you'll have to recompile sentilo-catalog-web redeploy the sentilo-catalog-web.war after that.


I created a provider and immediately after that, an observation using the new provider’s token is rejected with 401 “Invalid credential”
----------------------------------------------------------------------------------------------------------------------------------------

The providers are activated in a background job that runs every 5
minutes. Please wait a moment :-)

Another possible reason is that the Sentilo API server started before the Catalog application (probably deployed on your Tomcat).
At startup, the API server performs a call to /sentilo-catalog-web/api/entities/permissions in order to mirror the permissions stored in MongoDB with Redis.
If this call fails because the sentilo-catalog-web is not deployed yet, the permissions are not correctly created.
To resolve the issue, reboot your Sentilo and ensure that the API server starts always after the sentilo-catalog-web is fully deployed.

--------------

The command mvn package appassembler:assemble fails.
----------------------------------------------------

You have to execute the command in the directory of the component you
want to install.

--------------

I think I installed Sentilo. How can I confirm all is up & running?.
--------------------------------------------------------------------

You can use this script:

::

   ./scripts/testServerStatus.sh

You also might want to check `Platform
Testing <./platform_testing.html>`__

If you installed everything on your local machine, you can access the
catalog at http://localhost:8080/sentilo-catalog-web and the REST API at
http://localhost:8081

--------------

How can I activate debug logs?
------------------------------

You can pass the property :literal:`sentilo.log.level` to the JVM.
For example, you might add the following code to the script in the target/appassembler/bin
of the component you want to debug:

::

   -Dsentilo.log.level=DEBUG

