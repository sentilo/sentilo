// Script with the minimum data to run the test process: application, providers, permissions, sensor and component types.
// Beware: init_data.js must be executed before run this script

//Insert applications
print("Load applications");
db.application.insert({ "_id" : "testApp", "_class" : "org.sentilo.web.catalog.domain.Application", "name" : "testApp", "token" : "646967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f", "description" : "Platform test app", "email" : "sentilo@sentilo.org", "createdAt" : ISODate("2013-02-22T10:20:19.963Z"), "authorizedProviders" : [ ] });

// Insert providers
print("Load providers");
db.provider.insert({ "_id" : "testApp_provider", "_class" : "org.sentilo.web.catalog.domain.Provider", "name" : "testApp_provider", "token" : "563093ec5252147edc8860c2d667be5db0c010325b6953ed5b323724bcc00e05", "description" : "Provider to do integration tests", "createdAt" : ISODate("2013-03-15T08:48:42.966Z"), "contact" : { "name" : "Sentilo", "email" : "sentilo@sentilo.org" }});

// Insert alerts
print("Load alerts");
db.alert.insert({ "_id" : "testAlert", "_class" : "org.sentilo.web.catalog.domain.Alert", "name" : "testAlert", "description" : "Alert to do integration tests. Do not remove.", "createdAt" : ISODate("2015-05-18T10:49:23.438Z"), "type" : "EXTERNAL", "providerId" : "testApp_provider", "active" : true })

// Insert permissions
print("Load permissions");
db.permission.insert({ "_id" : "testApp_provider@testApp_provider", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "testApp_provider", "target" : "testApp_provider", "type" : "ADMIN" });
db.permission.insert({ "_id" : "sentilo-catalog@testApp_provider", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "sentilo-catalog", "target" : "testApp_provider", "type" : "ADMIN" });
db.permission.insert({ "_id" : "sentilo-catalog@testApp", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "sentilo-catalog", "target" : "testApp", "type" : "ADMIN" });
db.permission.insert({ "_id" : "testApp@testApp_provider", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "testApp", "target" : "testApp_provider", "type" : "READ" });
db.permission.insert({ "_id" : "testApp@testApp", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "testApp", "target" : "testApp", "type" : "ADMIN" });

// Insert component types
print("Load component types");
db.componentType.insert({ "_id" : "generic", "_class" : "org.sentilo.web.catalog.domain.ComponentType", "name" : "Generic", "description" : "Generic component type", "icon" : "pins1", "createdAt" : ISODate("2013-11-08T10:28:01.226Z") });
db.componentType.insert({ "_id" : "electricity_meter", "_class" : "org.sentilo.web.catalog.domain.ComponentType", "name" : "Electricity meter", "icon" : "pins2", "createdAt" : ISODate("2013-11-08T10:28:01.226Z") });
db.componentType.insert({ "_id" : "noise", "_class" : "org.sentilo.web.catalog.domain.ComponentType", "name" : "Noise meter", "icon" : "pins3", "createdAt" : ISODate("2013-11-08T10:28:01.226Z") });
db.componentType.insert({ "_id" : "meteo", "_class" : "org.sentilo.web.catalog.domain.ComponentType", "name" : "Meteo", "icon" : "pins4", "createdAt" : ISODate("2013-11-08T10:28:01.226Z") });

// Insert sensor types
print("Load sensor types");
db.sensorType.insert({ "_id" :"temperature", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Temperature",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"noise", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Noisemeter",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"anemometer", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Anenometer",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"humidity", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Humidity",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"pluviometer", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Pluviometer",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"rain", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Rain", "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
db.sensorType.insert({ "_id" :"wind", "_class" : "org.sentilo.web.catalog.domain.SensorType", "name" : "Wind",  "createdAt" : ISODate("2013-11-08T10:27:36.152Z") });
