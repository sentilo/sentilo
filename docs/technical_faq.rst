Technical FAQ
=============

In which platforms has been Sentilo tested ?
--------------------------------------------

The first deployment for the Barcelona City Council has been tested in
the following infraestructure:

-  Four virtual machines, two for the front-ends and another two for the
   back-end
-  All of them use as operating system Ubuntu server LTS 12.04
-  The real time database server(Redis) works with 16 GB of memory and
   36 GB of hard disk
-  The other three servers works with 4 GB of memory and 16 GB of hard
   disk

Another deployment configuration should work properly, always keeping in
mind the expected load by the system. There is also a `virtual
machine <./use_a_virtual_machine.html>`__ ready for use that can be used
for testing purposes.

I successfully published an observation, but I cannot see the data in catalog.
------------------------------------------------------------------------------

Check that the Catalog and Sentilo API Server are in the same timezone,
for example in UTC. Make sure the sentilo-server script has the
following VM option:

::

   -Duser.timezone=UTC

Also, make sure that the Tomcat that hosts the Catalog application has
the same option, for example en $JAVA_OPTS variable.

--------------

Maps is not showing up in Catalog application
---------------------------------------------

Recently Google changed it policy regarding Maps key. Please go to
https://developers.google.com/maps/documentation/javascript/get-api-key
and create one.

If you are using the last release of Sentilo(1.6) you can define the API
key inside the catalog-config.properties configuration file:

.. code:: properties

   # Google API key to use Google Maps
   google.api.key=<your key> 

--------------

I created a provider and immediately after that, an observation using the new provider’s token is rejected with 401 “Invalid credential”
----------------------------------------------------------------------------------------------------------------------------------------

The providers are activated in a background job that runs every 5
minutes. Please wait a moment :-)

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
