// By default, set each entity as active
db.provider.update({},{ $set: {"active": true}}, {multi:true});
db.application.update({},{ $set: {"active": true}}, {multi:true});

// Update application fields to add new contact structure (same as provider's contact field) 
db.application.find().forEach(function(e) {    
	var email = e.email === undefined ? "" : e.email;
    db.application.update({"_id": e._id},{$set:  {"contact" : { "email":email, "name": ""}}});
});
db.application.update({}, {$unset: {email:1}} , {multi: true});