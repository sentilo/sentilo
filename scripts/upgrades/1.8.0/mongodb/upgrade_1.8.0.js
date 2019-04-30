// Remove the unnecessary field 'passwordRepeat' from the User collection. 
db.user.update({},{ $unset: {"passwordRepeat": ""}}, {multi:true});

//Previous versions had a bug when a new user was added to the Catalog by a super admin user: both fields tenantsAuth 
//and tenantsListVisible were initialized incorrectly. Therefore, in some situations, these users were not 
//displayed in the admin list.
var total = 0;
var users = db.user.find({tenantsAuth:{$exists:true,$eq:[]}, tenantId:{$exists:true}});
users.forEach(function (e) {	
var updated = false;
	
if((e.tenantsAuth && e.tenantsAuth.indexOf(e.tenantId)) || !e.tenantsAuth){
	db.user.update({"_id" : e._id}, { $push: { "tenantsAuth" : e.tenantId}} );
	print("tenantsAuth field for user " + e.name + " updated!")
	updated = true;
}

if((e.tenantsListVisible && e.tenantsListVisible.indexOf(e.tenantId)) || !e.tenantsListVisible){
	db.user.update({"_id" : e._id}, { $push: { "tenantsListVisible" : e.tenantId}} );
	print("tenantsListVisible field for user " + e.name + " updated!")
	updated = true;
}        			  

if(updated){ 	
	total++;
}

});
print("Number of users updated: " + total);
