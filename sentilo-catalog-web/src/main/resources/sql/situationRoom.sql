
CREATE TABLE psab_component (
	id VARCHAR(128) NOT NULL,
	name VARCHAR(128) NOT NULL,
	provider VARCHAR(128) NOT NULL,
	latitude VARCHAR(50),
	longitude VARCHAR(50),
	compType VARCHAR(128) NOT NULL,
	description VARCHAR(128),	
	PRIMARY KEY(id));

CREATE TABLE psab_sensor (
	id VARCHAR(128) NOT NULL,
	sensor VARCHAR(128) NOT NULL,
	provider VARCHAR(128) NOT NULL,
	component VARCHAR(128) NOT NULL,
	dataType VARCHAR(25) NOT NULL,
	sensorType VARCHAR(128) NOT NULL,
	unit VARCHAR(25) NOT NULL,
	description VARCHAR(128),	
	PRIMARY KEY(id));