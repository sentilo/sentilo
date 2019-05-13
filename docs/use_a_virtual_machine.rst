Use a Virtual Machine
=====================

A Sentilo sample instance is available for testing purposes distributed
as a Open Virtual Appliance file
(`OVA <https://en.wikipedia.org/wiki/Open_Virtualization_Format>`__).

The appliance contains the **1.7.0 Sentilo release**.

Components installed:

-  Sentilo Catalog (web application)
-  Sentilo Platform Server (REST API)
-  Sentilo Relational Agent (saves the data to mySQL)
-  Sentilo Alert Agent
-  Sentilo Location Updater Agent

Two different distribution files are available:

-  One designed for **Virtual Box**, available
   `here <http://www.sentilo.io/wordpress/?wpfb_dl=24>`__. It has been
   tested using version **5.0.24**.
-  The second one, built for **ESXI** systems, available
   `here <http://www.sentilo.io/wordpress/?wpfb_dl=25>`__. It has been
   tested using **VMPlayer 12.5.5** and **ESXI 6.0**.

Please, keep in mind some important facts:

-  The virtual machine credentials are **sentilo/sentilo**.
-  You should config the network type as **“Bridged Adapter”.**
-  When stopping the virtual machine, it should be done in a organized
   way, in a Virtual Box environment you have to do this using the
   option **“Shutdown ACPI”**. You could also do this from the command
   line executing **“sudo shutdown -h now”**

After the virtual machine is started, all the sentilo services are
launched automatically. The IP of the virtual machine is assigned
automatically, to know which one is, enter into virtual machine and
execute the **“ifconfig”** conmmand. In some settings you might need to
port forward guest ports (essentially 8080 and 8081) and access them
from your host machine.

First steps:

-  Review the README file located in /home/sentilo.
-  The Catalog Console webapp will be ready to access in:
   http://your_ip:8080/sentilo-catalog-web/ with a access credentials:
   admin/1234
-  The API Rest endpoint will be listening for requests in:
   http://your_ip:8081
