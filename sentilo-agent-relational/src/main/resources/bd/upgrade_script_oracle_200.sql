/** Add new columns SENSOR_TYPE, COMPONENT and COMPONENT_TYPE to the 3 tables*/
ALTER TABLE sentilo_observations ( 
	ADD sensor_type VARCHAR(128)L, 
	ADD component VARCHAR(128) ,
	ADD component_type VARCHAR(128) ,
	ADD publisher_tenant VARCHAR(128) ,
    ADD tenant VARCHAR(128)
);
ALTER TABLE sentilo_orders ( 
	ADD sensor_type VARCHAR(128) , 
	ADD component VARCHAR(128) ,
	ADD component_type VARCHAR(128) ,
	ADD publisher_tenant VARCHAR(128) ,
    ADD tenant VARCHAR(128)
);
ALTER TABLE sentilo_alarms ( 	
	RENAME COLUMN alarm TO alert,
	ADD alert_type VARCHAR(12),
	ADD provider VARCHAR(128),
	ADD sensor VARCHAR(128) ,
	ADD sensor_type VARCHAR(128) , 
	ADD component VARCHAR(128) ,
	ADD component_type VARCHAR(128) ,
	ADD location VARCHAR(50) ,
	ADD publisher_tenant VARCHAR(128) ,
    ADD tenant VARCHAR(128)
);