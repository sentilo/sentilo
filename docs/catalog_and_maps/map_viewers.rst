Map Viewers
-----------

In the publicly accessible section of the Catalog, Sentilo offers two different map viewers that display
data in real-time, as they are being published in the system.

Both viewers are accessible from the *Explore* item
at the top menu bar (*Universal viewer* and *Route viewer*).


Universal viewer
~~~~~~~~~~~~~~~~

The map system is prepared to be displayed through various managers and cartography rendering servers. 
The default system is *Open Street Map* powered by *Leaflet*. You can find more information in the section  
:ref:`Map Providers`

Components map
^^^^^^^^^^^^^^

The catalog provides a default map, which shows
all the public components registered at the platform. If the user is
logged as administrator, all the private components will be displayed as
well.

.. image:: /_static/images/catalog_and_maps/universal_viewer_001.png


Component types filter
''''''''''''''''''''''

On this page, you can filter the components to show by selecting a
*component type* by clicking on the left top menu button |universal_viewer_menu_button.png|, 
that will expand the left sidebar:

.. image:: /_static/images/catalog_and_maps/universal_viewer_002.png

From here, you can select or deselect **component typologies** and **categories** (a *category* 
is a grouping of typologies), and or select / unselect all the categories / typologies (by checking / 
unchecking the *All* checkbox).

Check *All*:

.. image:: /_static/images/catalog_and_maps/universal_viewer_003.png

Uncheck *All*:

.. image:: /_static/images/catalog_and_maps/universal_viewer_004.png

In addition, it is also possible to perform a search within the typology tree. Writing in the upper 
text field we will obtain a list of only those typologies that match the searched text:

.. image:: /_static/images/catalog_and_maps/universal_viewer_005.png

.. note::

   The **organization by categories** is achieved through the *tags of the component type*. 
   That is, a typology belongs to each of the categories added as tags to that typology. 
   For more information, consult the section: `Component types <../catalog_and_maps/administration_console.html#component-types>`_


Map controls
''''''''''''

Apart from the button to open the left sidebar, on the universal map we have 6 more buttons, all located on the right side.

In order from top to bottom:

- **Search on map (magnifying glass icon)**: hover your mouse over this icon to display a text field with which you can search for an address or point of interest on the map, which will be centered after the correct result
- **Locate me (arrow icon)**: locate your position on the map (requires location permissions on the browser)
- **Maximize/Minimize map (axpand/unexpand icon)**: maximize or minimize the map in full screen (F11 effect)
- **Zoom in / Zoom out (plus / minus icons)**: zoom in or out on the map
- **Change map layers (layers icon)**: changes the layer represented on the map (it will show each of the layers configured in the catalog application, see :ref:`Map Providers`)

Component concentration
'''''''''''''''''''''''

In case there is a high concentration of pois in the same point, the map will show a considerable agglomeration of them:

.. image:: /_static/images/catalog_and_maps/universal_viewer_007.png

There is the particularity that if you click on one of them, the whole set of pois in esperial will open, to be able to select them in a simple and clear way:

.. image:: /_static/images/catalog_and_maps/universal_viewer_008.png



Component details
^^^^^^^^^^^^^^^^^

Sensors list
''''''''''''

When you select a component, a popup window is opened above the map and
displays the list of sensors related to it with the last activity for
each one of them (as noted above, the private sensors will be displayed
only for logged users):

.. image:: /_static/images/catalog_and_maps/component_popup_001.png

Sensors last activity view
''''''''''''''''''''''''''

If you click into the content area of the popup window, a new page is
open displaying some basic details about the component, and a
time-series graph with the last activity of each of its sensors:

.. image:: /_static/images/catalog_and_maps/component_map_extended_details_001.png

You may also click the bottom-right corner icon |universal_viewer_006.png| 
and get a page with bigger detail. The URL of this page is shareable, i.e. it is possible to send it by email.

.. image:: /_static/images/catalog_and_maps/component_map_extended_details_002.png

.. _navigate-the-last-data-chart-1:

Navigate the last data chart

You can navigate along the dates of the graph by using the buttons
located in the lower right corner of it:

.. image:: /_static/images/catalog_and_maps/chart_controls.png

-  **left arrow**: navigate to the past (only if there are older data)
-  **reload data (center button)**: reload last data / reset chart data
-  **right arrow**: navigate to the future (only if you have navigated
   or gone into the past before)


Displaying complex data
^^^^^^^^^^^^^^^^^^^^^^^

In some cases, you may want to inform **complex data** as an observation
on Sentilo, such like a large json object. For these cases, Sentilo will
detect that the text is a json object and then it will be shown to you
as a prettify json value:

.. image:: /_static/images/catalog_and_maps/complex_data_001.png

You can expand or compress the prettified json with the bottom buttons
under the status field.


Route viewer
~~~~~~~~~~~~

As the name suggest, the route viewer is a specific map that shows the
routes followed by the mobile components (keep in mind that only the
last 20 points are displayed for each route):

.. image:: /_static/images/catalog_and_maps/routes_viewer_001.png

The same features described previously apply on this map and its markers
(popup window, … ), but with the particularity that if you click over a
*route point* then the popup window displays sensor activity related to
the time instant in which component was at that location.

.. image:: /_static/images/catalog_and_maps/routes_viewer_002.png


Background map configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Center and Zoom
^^^^^^^^^^^^^^^

Each Organization can have it's own map center and zoom level.
Please refer to corresponding part in the Administration Console section:
`Organization Map configuration <../administration_console.html#map-configuration>`_

.. _Map Providers:

Map Providers
^^^^^^^^^^^^^

For all background maps, you can use either Leaflet (by default) or other map provider. 
Through the latter you can consume any OGC WMS/WMTS service that provides a EPSG 3857 SRID.

The configuration is in :literal:`/sentilo-catalog-web/src/main/resources/properties/sentilo-catalog.conf`.

Example of using Google Maps provider (you must provide the Google Maps Key):

::

	# Google API key to use Google Maps
	sentilo.catalog.map.provider=gmaps
	sentilo.catalog.map.google.key=

Example of the default Leaflet maps configuration, using a public Open Street Maps WMS service:

::

	# Maps config
	sentilo.catalog.map.provider=leaflet
	sentilo.catalog.map.wms.layers=[\
    	{"name":"Open Street Maps","url":"http://{s}.tile.osm.org/{z}/{x}/{y}.png","layer":"","version":"1.3.0","format":"image/png","attribution":"Open Street Maps (OSM)","styles":""}
	]


.. image:: /_static/images/catalog_and_maps/map_providers_001.png

.. note::

   If you insert multiple layers in the *catalog.map.wms.layers* property, a layer selector
   in the bottom-right corner of the map will appear.





.. |universal_viewer_006.png| image:: ../_static/images/catalog_and_maps/universal_viewer_006.png
.. |universal_viewer_menu_button.png| image:: ../_static/images/catalog_and_maps/universal_viewer_menu_button.png