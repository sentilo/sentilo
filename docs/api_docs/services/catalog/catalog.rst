Catalog
=======

.. toctree::
   :hidden:
   :titlesonly:
   :maxdepth: 1
   
   create_sensors
   update_sensors
   retrieve_authorized_entities
   delete_component_or_sensor



Description
-----------

The catalog service allows to register or modify your own
sensors/components or query the characteristics of a sensor or provider.

All requests for this service will have the following format:

::

    http://<your_api_server.com>/catalog/<provider_id>

where provider_id is optional and should be included depending on the
operation.

Actions
-------

The available actions for this service are:

-  `Adding components / sensors <./create_sensors.html>`__
-  `Update data components / sensors <./update_sensors.html>`__
-  `Retrieve list of providers /
   sensors <./retrieve_authorized_entities.html>`__
-  `Remove components / sensors <./delete_component_or_sensor.html>`__

Component types
---------------

The list of component types should be configured for each Sentilo
instance, the following list could be used as a reference for any city:

+-----------------------+-----------------------+-----------------------+
| Id                    | Name                  | Description           |
+=======================+=======================+=======================+
| temperature           | Temperature           | Temperature           |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| noise                 | Soundmeter            | Sound measurement     |
+-----------------------+-----------------------+-----------------------+
| wind                  | Anemometer            | Wind speed            |
+-----------------------+-----------------------+-----------------------+
| humidity              | Humidity              | Humidity measurement  |
+-----------------------+-----------------------+-----------------------+
| air_quality           | Air Quality           | Air Quality control   |
+-----------------------+-----------------------+-----------------------+
| water_quality         | Water Quality         | Water Quality control |
+-----------------------+-----------------------+-----------------------+
| meteo                 | Meteorology           | Weather Station       |
+-----------------------+-----------------------+-----------------------+
| parking               | Occupation parking    | Parking control       |
+-----------------------+-----------------------+-----------------------+
| luminosity            | Luminosity            | Luminosity            |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| glass_container       | Occupancy container   | Glass occupancy       |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| paper_container       | Occupancy container   | Paper occupancy       |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| plastic_container     | Occupancy container   | Plastic occupancy     |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| organic_container     | Occupancy container   | Organic occupancy     |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| refuse_container      | Occupancy container   | Refuse occupancy      |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| container_volum       | Occupancy container   | Generic occupancy     |
|                       | level                 | container measurement |
+-----------------------+-----------------------+-----------------------+
| soil_moisture         | Soil moisture         | Soil moisture         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| park_meter            | Parking meter         | Parking meter control |
+-----------------------+-----------------------+-----------------------+
| traffic               | Traffic               | Traffic measurement   |
+-----------------------+-----------------------+-----------------------+
| people_flow           | People flow           | Pedestrian flow       |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| flowmeter             | Water flow            | Water flow            |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| solenoid_valve        | Electrovalve          | Solenoid control      |
+-----------------------+-----------------------+-----------------------+
| salinity              | Salinity              | Soil salinity         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| internal_ambient_cond | Internal              | Temperature, humidity |
| itions                | Environmental         | and luminosity        |
|                       | Conditions            | measurement           |
+-----------------------+-----------------------+-----------------------+
| external_ambient_cond | External              | Temperature, humidity |
| itions                | environmental         | and luminosity        |
|                       | conditions            | measurement           |
+-----------------------+-----------------------+-----------------------+
| network_analyzer      | Network analyzer      | Electric network      |
|                       |                       | analyzer              |
+-----------------------+-----------------------+-----------------------+
| gas_meter             | Gas meter             | Gas consumption meter |
+-----------------------+-----------------------+-----------------------+
| electricity_meter     | Electricity meter     | Electricity           |
|                       |                       | consumption meter     |
+-----------------------+-----------------------+-----------------------+
| water_meter           | Water meter           | Water consumption     |
|                       |                       | meter                 |
+-----------------------+-----------------------+-----------------------+
| soil_sensor           | Soil sensor           | Soil mesurement of    |
|                       |                       | salinity, mosture,    |
|                       |                       | etc                   |
+-----------------------+-----------------------+-----------------------+
| generic               | Generic component     | Default component     |
|                       | type                  | type if not specified |
+-----------------------+-----------------------+-----------------------+
| plugsense             | Plug & Sense          | Plug & Sense Libelium |
|                       |                       | component             |
+-----------------------+-----------------------+-----------------------+

Sensor types
------------

The list of sensor types should be configured for each Sentilo instance,
the following list could be used as a reference for any city:

+-----------------------+-----------------------+-----------------------+
| Id                    | Name                  | Description           |
+=======================+=======================+=======================+
| temperature           | Temperature           | Temperature           |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| noise                 | Soundmeter Class II   | Sound level measuring |
|                       |                       | class II.             |
+-----------------------+-----------------------+-----------------------+
| noise_class_i         | Soundmeter Class I    | Sound level measuring |
|                       |                       | class I               |
+-----------------------+-----------------------+-----------------------+
| anemometer            | Anemometer            | Wind Speed ​measuring |
+-----------------------+-----------------------+-----------------------+
| humidity              | Humidity              | Humidity measuring    |
+-----------------------+-----------------------+-----------------------+
| parking               | Occupation parking    | Occupation parking    |
|                       |                       | control               |
+-----------------------+-----------------------+-----------------------+
| luminosity            | Luminosity            | Luminosity measuring  |
+-----------------------+-----------------------+-----------------------+
| container_volum       | Occupancy container   | Occupancy container   |
|                       | level                 | measurement           |
+-----------------------+-----------------------+-----------------------+
| container_overturn    | Container overturned  | Container overturned  |
|                       |                       | indicator             |
+-----------------------+-----------------------+-----------------------+
| container_open        | Container open        | Container opening     |
|                       |                       | indicator             |
+-----------------------+-----------------------+-----------------------+
| status                | Sensor status         | Status control        |
+-----------------------+-----------------------+-----------------------+
| battery               | Battery level         | Battery level         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| soil_moisture_15      | Soil moisture 15 cm.  | Soil moisture         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| soil_moisture_35      | Soil moisture 35 cm.  | Soil moisture         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| park_meter            | Parking meter         | Parking meter control |
+-----------------------+-----------------------+-----------------------+
| vehicle_volume        | Number of vehicles    | Measurement of number |
|                       |                       | of vehicles           |
+-----------------------+-----------------------+-----------------------+
| vehicle_occupation_av | Average occupancy     | Measurement of        |
| erage                 |                       | average occupancy in  |
|                       |                       | vehicles              |
+-----------------------+-----------------------+-----------------------+
| vehicle_speed         | Speed ​​Vehicle       | Vehicle speed         |
|                       |                       | ​​measurement         |
+-----------------------+-----------------------+-----------------------+
| air_quality_no2       | NO2                   | Nitrogen dioxide      |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| air_quality_pm10      | PM10                  | Measurement of        |
|                       |                       | suspension particles  |
|                       |                       | PM10                  |
+-----------------------+-----------------------+-----------------------+
| air_quality_pm25      | PM25                  | Measurement of        |
|                       |                       | supsension particles  |
|                       |                       | PM25                  |
+-----------------------+-----------------------+-----------------------+
| air_quality_o3        | O3                    | Ozone measurement     |
+-----------------------+-----------------------+-----------------------+
| air_quality_so2       | SO2                   | Sulfur dioxide        |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| air_quality_co        | CO                    | Carbon Monoxide       |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| air_quality_co2       | CO2                   | Carbon dioxide        |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| people_flow           | People flow           | Measurement of        |
|                       |                       | pedestrian flow       |
+-----------------------+-----------------------+-----------------------+
| flowmeter             | Water flow            | Water flow            |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| solenoid_valve        | Electrovalve          | Solenoid actuator     |
+-----------------------+-----------------------+-----------------------+
| eto                   | Evotranspiration      | Evotranspiration      |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| salinity              | Salinity              | Soil salinity         |
|                       |                       | measurement           |
+-----------------------+-----------------------+-----------------------+
| pluviometer           | Pluviometer           | Rain measurement      |
+-----------------------+-----------------------+-----------------------+
| rain                  | Rain gauge            | Rain indicator (it's  |
|                       |                       | raining/it's not      |
|                       |                       | raining)              |
+-----------------------+-----------------------+-----------------------+
| wind                  | Wind gauge            | Wind indicator (>X    |
|                       |                       | m/s)                  |
+-----------------------+-----------------------+-----------------------+
| wind_direction_6_m    | Wind direction        | Wind direction at 6   |
|                       |                       | meters                |
+-----------------------+-----------------------+-----------------------+
| wind_direction_10_m   | Wind direction        | Wind direction at 10  |
|                       |                       | meters                |
+-----------------------+-----------------------+-----------------------+
| voltage               | Voltmetre             | Electrical voltage    |
|                       |                       | measurement.          |
|                       |                       | Units: volts (V)      |
+-----------------------+-----------------------+-----------------------+
| current               | Ammeter               | Electrical intensity  |
|                       |                       | measurement.          |
|                       |                       | Units amps (A)        |
+-----------------------+-----------------------+-----------------------+
| frequency             | Frequencymeter        | Electrical frequency  |
|                       |                       | measurement.          |
|                       |                       | Units: herzs (Hz)     |
+-----------------------+-----------------------+-----------------------+
| active_power          | Active power          | Active power          |
|                       |                       | measurement.          |
|                       |                       | Units: kilowatts (kW) |
+-----------------------+-----------------------+-----------------------+
| reactive_power        | Reactive power        | Reactive power        |
|                       |                       | measurement.          |
|                       |                       | Units: reactive       |
|                       |                       | kilovoltiamperis      |
|                       |                       | (kvar)                |
+-----------------------+-----------------------+-----------------------+
| cosphi                | Power factor          | Sensor that relates   |
|                       |                       | the active and        |
|                       |                       | reactive power. No    |
|                       |                       | units                 |
+-----------------------+-----------------------+-----------------------+
| active_energy         | Active electrical     | Measurement of        |
|                       | energy meter          | accumulated active    |
|                       |                       | power.                |
|                       |                       | Units: kWh.           |
+-----------------------+-----------------------+-----------------------+
| reactive_energy       | Reactive electrical   | Measurement of        |
|                       | energy meter          | accumulated reactive  |
|                       |                       | power.                |
|                       |                       | Units: kvarh.         |
+-----------------------+-----------------------+-----------------------+
| gas_volume            | Gas meter             | Measurement of        |
|                       |                       | accumulated gas       |
|                       |                       | consumption.          |
|                       |                       | Units: m3 o Nm3       |
+-----------------------+-----------------------+-----------------------+
| water_meter           | Water meter           | Measurement of        |
|                       |                       | accumulated water     |
|                       |                       | consumption.          |
|                       |                       | Units: m3 o l         |
+-----------------------+-----------------------+-----------------------+
| global_solar_irradian | Global solar          | Mesurement of solar   |
| ce                    | irradiance            | irradiance            |
+-----------------------+-----------------------+-----------------------+
| leaf_moisture         | Leaf moisture         | Leaf wetness          |
+-----------------------+-----------------------+-----------------------+
| oxygen                | Oxygen                | O2                    |
+-----------------------+-----------------------+-----------------------+
| vertical_level        | Vertical level        | Vertical liquid level |
|                       |                       | (water)               |
+-----------------------+-----------------------+-----------------------+
| bend                  | Bend                  | Bend                  |
+-----------------------+-----------------------+-----------------------+
| lpg                   | Lpg                   | Liquified petroleum   |
|                       |                       | gases (H2, CH4,       |
|                       |                       | ethanol & isobutane)  |
+-----------------------+-----------------------+-----------------------+
| crack_detection       | Crack detection       | Crack detection gauge |
+-----------------------+-----------------------+-----------------------+
| solar_radiation       | Solar radiation       | Solar radiation       |
+-----------------------+-----------------------+-----------------------+
| voc                   | Voc                   | Volatile Organic      |
|                       |                       | Compounds             |
+-----------------------+-----------------------+-----------------------+
| chloride_ion          | Chloride ion          | Ion Cl-               |
+-----------------------+-----------------------+-----------------------+
| temperature           | Temperature           | Soil/Water            |
|                       |                       | temperature           |
+-----------------------+-----------------------+-----------------------+
| magnessium_ion        | Magnessium ion        | Ion Mg2+              |
+-----------------------+-----------------------+-----------------------+
| distance              | Distance              | Distance (by metalic  |
|                       |                       | pressure or by        |
|                       |                       | pressure)             |
+-----------------------+-----------------------+-----------------------+
| conductivity          | Conductivity          | Conductivity          |
+-----------------------+-----------------------+-----------------------+
| liquid_leakage_line   | Liquid leakage line   | Water Leakage /       |
|                       |                       | Liquid Detection      |
|                       |                       | (Line)                |
+-----------------------+-----------------------+-----------------------+
| crack_propagation     | Crack propagation     | Crack propagation     |
|                       |                       | gauge                 |
+-----------------------+-----------------------+-----------------------+
| nitrate_ion           | Nitrate ion           | Ion NO3               |
+-----------------------+-----------------------+-----------------------+
| hall_effect           | Hall effect           | Hall effect           |
+-----------------------+-----------------------+-----------------------+
| vibration             | Vibration             | Vibration (lamina)    |
+-----------------------+-----------------------+-----------------------+
| copper_ion            | Copper ion            | Ion Cu2+              |
+-----------------------+-----------------------+-----------------------+
| calcium_ion           | Calcium ion           | Ion Ca+               |
+-----------------------+-----------------------+-----------------------+
| dendometer            | Dendometer            | Trunk, stem or fruit  |
|                       |                       | diameter              |
+-----------------------+-----------------------+-----------------------+
| iodide_ion            | Iodide ion            | Ion I-                |
+-----------------------+-----------------------+-----------------------+
| bromide_ion           | Bromide ion           | Ion Br-               |
+-----------------------+-----------------------+-----------------------+
| sodium_ion            | Sodium ion            | Ion Na+               |
+-----------------------+-----------------------+-----------------------+
| linear_displacement   | Linear displacement   | Linear displacement   |
+-----------------------+-----------------------+-----------------------+
| atmospheric_pressure  | Atmospheric pressure  | Atmospheric pressure  |
+-----------------------+-----------------------+-----------------------+
| methane               | Methane               | CH4                   |
+-----------------------+-----------------------+-----------------------+
| pressure              | Pressure              | Pressure/ Weight      |
+-----------------------+-----------------------+-----------------------+
| ammonia               | Ammonia               | NH3                   |
+-----------------------+-----------------------+-----------------------+
| redox_potential       | Redox potential       | Oxidation Reduction   |
|                       |                       | Potential             |
+-----------------------+-----------------------+-----------------------+
| proximity_indoor      | Proximity indoor      | Ultrasound (indoor)   |
+-----------------------+-----------------------+-----------------------+
| air_pollutant         | Air pollutant         | Air pollutants-I      |
|                       |                       | (NH3, SH2, ethanol    |
|                       |                       | and toluene) and air  |
|                       |                       | pollutants-II (H2,    |
|                       |                       | CH4, CO, ethanol and  |
|                       |                       | isobutane)            |
+-----------------------+-----------------------+-----------------------+
| liquid_leakage_point  | Liquid leakage point  | Water Leakage /       |
|                       |                       | Liquid Detection      |
|                       |                       | (Point)               |
+-----------------------+-----------------------+-----------------------+
| proximity_outdoor     | Proximity outdoor     | Ultrasound (outdoor   |
|                       |                       | IP67)                 |
+-----------------------+-----------------------+-----------------------+
| potassium_ion         | Potassium ion         | Ion K+                |
+-----------------------+-----------------------+-----------------------+
| o_saturation          | O saturation          | Dissolved Oxygen      |
+-----------------------+-----------------------+-----------------------+
| presence              | Presence              | Presence (PIR)        |
+-----------------------+-----------------------+-----------------------+
| stretch               | Stretch               | Stretch               |
+-----------------------+-----------------------+-----------------------+
| liquid_level          | Liquid level          | Horizontal liquid     |
|                       |                       | level (combustibles   |
|                       |                       | or water)             |
+-----------------------+-----------------------+-----------------------+
| load                  | Load                  | Load                  |
+-----------------------+-----------------------+-----------------------+
| fluoride_ion          | Fluoride ion          | Ion F-                |
+-----------------------+-----------------------+-----------------------+
| ph                    | Ph                    | pH                    |
+-----------------------+-----------------------+-----------------------+
| solvent_vapors        | Solvent vapors        | Solvent vapors (H2,   |
|                       |                       | CH4, CO, ethanol and  |
|                       |                       | isobutane)            |
+-----------------------+-----------------------+-----------------------+
| accelerometer         | Accelerometer         | Accelerometer         |
+-----------------------+-----------------------+-----------------------+

