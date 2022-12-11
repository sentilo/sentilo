-- Script de creacion del modelo de entidades para una BD Oracle: utiliza triggers para informar el id de cada registro
CREATE TABLE sentilo_observations (
	id NUMBER(10) NOT NULL,
	provider VARCHAR(128) NOT NULL,
	sensor VARCHAR(128),
	sensor_type VARCHAR(128),
	component VARCHAR(128),
	component_type VARCHAR(128),
	value VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp date NOT NULL,
	published_at date NOT NULL,
	publisher VARCHAR(128),
	location varchar(50),
	publisher_tenant varchar(128),
    tenant varchar(128),
	PRIMARY KEY(id));

CREATE TABLE sentilo_orders (
	id NUMBER(10) NOT NULL,
	provider VARCHAR(128) NOT NULL,
	sensor VARCHAR(128),
	sensor_type VARCHAR(128),
	component VARCHAR(128),
	component_type VARCHAR(128),
	message VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp date NOT NULL,
	published_at date NOT NULL,
	publisher VARCHAR(128),
	location varchar(50),
	publisher_tenant varchar(128),
    tenant varchar(128),
	PRIMARY KEY(id));

CREATE TABLE sentilo_alarms (
	id NUMBER(10) NOT NULL,
	alert VARCHAR(128) NOT NULL,
	alert_type VARCHAR(12),
	provider VARCHAR(128),
	sensor VARCHAR(128),
	sensor_type VARCHAR(128),
	component VARCHAR(128),
	component_type VARCHAR(128),
	message VARCHAR(512) NOT NULL,
	timestamp varchar(20) NOT NULL,
	event_timestamp date NOT NULL,
	published_at date NOT NULL,
	publisher VARCHAR(128),
	location VARCHAR(50),
	publisher_tenant VARCHAR(128),
    tenant VARCHAR(128),
	PRIMARY KEY(id));


-- Los secuenciales y los triggers que se definen a continuacion permiten delegar en la BD la generacion de cada uno de los ids	
CREATE SEQUENCE sentilo_observations_seq;	
CREATE SEQUENCE sentilo_orders_seq;
CREATE SEQUENCE sentilo_alarms_seq;


CREATE OR REPLACE TRIGGER sentilo_observations_trigger 
BEFORE INSERT ON sentilo_observations 
FOR EACH ROW
BEGIN
  SELECT sentilo_observations_seq.NEXTVAL
  INTO   :new.id
  FROM   dual;
END;

CREATE OR REPLACE TRIGGER sentilo_orders_trigger 
BEFORE INSERT ON sentilo_orders 
FOR EACH ROW
BEGIN
  SELECT sentilo_orders_seq.NEXTVAL
  INTO   :new.id
  FROM   dual;
END;

CREATE OR REPLACE TRIGGER sentilo_alarms_trigger 
BEFORE INSERT ON sentilo_alarms 
FOR EACH ROW
BEGIN
  SELECT sentilo_alarms_seq.NEXTVAL
  INTO   :new.id
  FROM   dual;
END;