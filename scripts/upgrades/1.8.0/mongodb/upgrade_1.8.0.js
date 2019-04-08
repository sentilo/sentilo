// Remove the unnecessary field 'passwordRepeat' from the User collection. 
db.user.update({},{ $unset: {"passwordRepeat": ""}}, {multi:true});

