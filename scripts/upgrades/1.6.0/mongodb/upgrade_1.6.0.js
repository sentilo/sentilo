//Remove role USER from admin user 
db.user.update({_id:"admin"},{$pull:{roles:{$in:["USER"]}}});

// By default, set the state field to online for each sensor 
db.sensor.update({},{$set:{state:'online'}}, {multi:true});

//By default, set the active field to true for each alert 
db.alert.update({},{$set:{active:true}}, {multi:true});

//Unset synchronization mark to force a resync
db.alert.update({},{$unset:{synchronized:""}}, {multi:true});
db.sensor.update({},{$unset:{synchronized:""}}, {multi:true});