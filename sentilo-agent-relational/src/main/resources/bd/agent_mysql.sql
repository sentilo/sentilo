-- Script de creacion del modelo de entidades para una BD MySQL: utiliza la funcion auto_increment para informar el id de cada registro
CREATE TABLE sentilo_observations (
	id INT NOT NULL AUTO_INCREMENT,
	provider VARCHAR(128) NOT NULL,
	sensor VARCHAR(128),
	value VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp timestamp NOT NULL,
	published_at timestamp NOT NULL,
	publisher VARCHAR(128),
	location varchar(50),
	PRIMARY KEY(id));

CREATE TABLE sentilo_orders (
	id INT NOT NULL AUTO_INCREMENT,
	provider VARCHAR(128) NOT NULL,
	sensor VARCHAR(128),
	message VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp timestamp NOT NULL,
	published_at timestamp NOT NULL,
	publisher VARCHAR(128),
	PRIMARY KEY(id));

CREATE TABLE sentilo_alarms (
	id INT NOT NULL AUTO_INCREMENT,
	alarm VARCHAR(128) NOT NULL,
	message VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp timestamp NOT NULL,
	published_at timestamp NOT NULL,
	publisher VARCHAR(128),
	PRIMARY KEY(id));
