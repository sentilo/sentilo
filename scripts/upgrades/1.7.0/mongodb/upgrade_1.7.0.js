// Set by default listVisible = true for all tenant permissions
db.tenantPermission.update({},{$set:{listVisible:true}}, {multi:true});

var collections = ["provider","component","sensor","alert","alertRule","application","documentFile","user"];

// Set by default tenantsListVisible equals to tenantsAuth for convenience
collections.forEach(function(collectionName) {
	db[collectionName].find().snapshot().forEach(
	  function (e) {
	    e.tenantsListVisible = e.tenantsAuth;
	    db[collectionName].save(e);
	  }
  );	  
  print("Collection " + collectionName + " updated.");

});
