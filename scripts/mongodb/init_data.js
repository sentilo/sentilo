//Insert applications
print("Load applications");
db.application.insert({ "_id" : "sentilo-catalog", "_class" : "org.sentilo.web.catalog.domain.Application", "name" : "sentilo-catalog", "token" : "c956c302086a042dd0426b4e62652273e05a6ce74d0b77f8b5602e0811025066", "description" : "Catalog application", "email" : "sentilo@sentilo.org", "createdAt" : ISODate("2013-11-08T13:15:01.930Z"), "authorizedProviders" : [ ] });

// Insert users
print("Load users");
db.user.insert({ "_id" : "admin", "_class" : "org.sentilo.web.catalog.domain.User", "password" : "1234", "name" : "Administrador", "description" : "", "email" : "sentilo@sentilo.org", "createdAt" : ISODate("2013-11-08T08:26:22.791Z"), "active" : true, "roles" : [ "ADMIN", "USER" ] });
db.user.insert({ "_id" : "platform_user", "_class" : "org.sentilo.web.catalog.domain.User", "password" : "sentilo", "passwordRepeat" : "sentilo", "name" : "Platform user", "description" : "PubSub platform user. Do not remove  it!.", "email" : "sentilo@sentilo.org", "createdAt" : ISODate("2013-11-08T13:42:40.848Z"), "active" : true, "roles" : [ "PLATFORM" ] });

// Insert permissions
print("Load permissions");
db.permission.insert({ "_id" : "sentilo-catalog@sentilo-catalog", "_class" : "org.sentilo.web.catalog.domain.Permission", "source" : "sentilo-catalog", "target" : "sentilo-catalog", "type" : "ADMIN" });