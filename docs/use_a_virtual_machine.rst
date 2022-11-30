Use a Virtual Machine
=====================

A Sentilo sample instance is available for testing purposes distributed
as a Open Virtual Appliance file
(`OVA <https://en.wikipedia.org/wiki/Open_Virtualization_Format>`__).

The appliance is available for download `here <https://drive.google.com/file/d/1pu1GorRtaNC9kY208Obt-9dUNoUlbEnZ/view?usp=sharing>`__.

It has been tested with **Virtual Box v5 and v6**.

.. note::

   Mac OS users might check if the downloaded file has the original :literal:`.ova` extension. If it has a :literal:`.ovf` extension,
   you have to rename it back to :literal:`.ova`, otherwise it possibly won't import to VirtualBox (the import will stuck forever).

The appliance contains the **1.9.0 Sentilo release** and runs Ubuntu Server 18.04.

Components installed:

-  Sentilo Catalog (web application)
-  Sentilo Platform Server (REST API)
-  Sentilo Relational Agent (saves the data to mySQL)
-  Sentilo Alert Agent
-  Sentilo Location Updater Agent

The virtual machine credentials are **sentilo/sentilo**.

All Sentilo services are started automatically. The REST API server starts 90 seconds after Tomcat,
because it needs to start after the Catalog is deployed. The services are exposed via NAT on localhost:

First steps:

-  Review the README file located in /home/sentilo.
-  The Catalog Console webapp will be ready to access in:
   http://localhost:8080/sentilo-catalog-web/ with a access credentials:
   admin/1234
-  The API Rest endpoint will be listening for requests in:
   http://localhost:8081
-  SSH server is listening on localhost 2222. Access credentials are sentilo/sentilo
