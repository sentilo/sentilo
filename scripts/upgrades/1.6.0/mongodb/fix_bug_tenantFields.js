// Previous versions had a bug when a new component is added to the Catalog: the field tenantsMapVisible
// is not initialized with the tenant value. Therefore, in some situations, components are not displayed
// in map viewer.


var total = 0;
  db.component.find().forEach(function (e) {
        if(e.tenantId){
		  var updated = false;
        	
          if((e.tenantsAuth && e.tenantsAuth.indexOf(e.tenantId)) || !e.tenantsAuth){
        	db.component.update({"_id" : e._id}, { $push: { "tenantsAuth" : e.tenantId}} );
 			print("tenantsAuth field for component " + e.name + " updated!")
 			updated = true;
          }
          
          if((e.tenantsMapVisible && e.tenantsMapVisible.indexOf(e.tenantId)) || !e.tenantsMapVisible){
         	db.component.update({"_id" : e._id}, { $push: { "tenantsMapVisible" : e.tenantId}} );
  			print("tenantsMapVisible field for component " + e.name + " updated!")
  			updated = true;
          }        			  
		  
		  if(updated){ 	
			total++;
		  }
        }
  });
  print("Components updated: " + total);