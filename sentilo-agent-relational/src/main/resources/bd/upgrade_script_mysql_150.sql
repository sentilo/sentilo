/** Add new column PUBLISHED_AT to the 3 tables*/
ALTER TABLE sentilo_observations ADD published_at TIMESTAMP NULL;
ALTER TABLE sentilo_orders ADD published_at TIMESTAMP NULL;
ALTER TABLE sentilo_alarms ADD published_at TIMESTAMP NULL;

/* To set the value of this column on the previous rows, you could execute:  */
update sentilo_observations set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;
update sentilo_alarms set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;
update sentilo_orders set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;