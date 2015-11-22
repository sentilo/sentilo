// Create a default tenant with default params (name Sentilo, location centered at Barcelona, ...)
db.tenant.insert({ "_id" : "sentilo", "_class" : "org.sentilo.web.catalog.domain.Tenant", "name" : "Sentilo", "description" : "Sentilo tenant", "isDefault": true, "contactName" : "Fill in your contact details", "contactEmail" : "fill_in@your.mail", "isPublic" : true, "mapParams" : { "zoomLevel" : 14, "center" : { "latitude" : 41.4001221, "longitude" : 2.172839 }}, "createdAt" : new ISODate(), "createdBy" : "sentilo"});

// Create a default super admin user 
db.user.insert({ "_id" : "sadmin", "_class" : "org.sentilo.web.catalog.domain.User", "password" : "change_it", "passwordRepeat" : "change_it", "name" : "SuperAdmin user", "description" : "SuperAdmin user.", "email" : "fill_in@your.mail", "createdAt" : new ISODate(), "active" : true, "roles" : [  "SUPER_ADMIN" ] });

// Rename updateAt field to updatedAt
var collections = ["activity","alert","application","component","componentType","permission","provider","sensor","sensorType","tenant","tenantPermission","user"];

collections.forEach(function(collectionName) {
  db[collectionName].update({},{ $rename: { "updateAt": "updatedAt" } }, {multi:true} );
  print("Collection " + collectionName + " updated.");

});
