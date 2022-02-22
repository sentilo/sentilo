-- Script de creacion del modelo de entidades para una BD PostgreSQL: utiliza la funcion SERIAL para informar el id de cada registro

CREATE TABLE sentilo_observations ( 
    id SERIAL PRIMARY KEY, 
    provider varchar(128) NOT NULL,
    sensor varchar(128),
    value varchar(512) NOT NULL,
    timestamp varchar(20) NOT NULL,
    event_timestamp timestamp NOT NULL, 
    published_at timestamp NOT NULL, 
    publisher varchar(128),
    location varchar(50));

CREATE TABLE sentilo_orders ( 
    id SERIAL PRIMARY KEY,
    provider varchar(128) NOT NULL,
    sensor varchar(128),
    message varchar(512) NOT NULL,
    timestamp varchar(20) NOT NULL,
    event_timestamp timestamp NOT NULL,
    published_at timestamp NOT NULL, 
    publisher varchar(128));

CREATE TABLE sentilo_alarms ( 
    id SERIAL PRIMARY KEY,
    alarm varchar(128) NOT NULL,
    message varchar(512) NOT NULL,
    timestamp varchar(20) NOT NULL,
    event_timestamp timestamp NOT NULL,
    published_at timestamp NOT NULL,
    publisher varchar(128));

