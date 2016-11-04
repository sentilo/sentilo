/** Add new column PUBLISHER to the 3 tables*/
ALTER TABLE sentilo_observations ADD (publisher VARCHAR(128));
ALTER TABLE sentilo_orders ADD (publisher VARCHAR(128));
ALTER TABLE sentilo_alarms ADD (publisher VARCHAR(128));

/** Add new column EVENT_TIMESTAMP to the 3 tables*/
ALTER TABLE sentilo_observations ADD event_timestamp date NULL;
ALTER TABLE sentilo_orders ADD event_timestamp date NULL;
ALTER TABLE sentilo_alarms ADD event_timestamp date NULL;

/* To set the value of this column on the previous rows, you should execute the following commands:  */
update sentilo_observations set event_timestamp = published_at where event_timestamp is null;
update sentilo_alarms set event_timestamp = published_at where event_timestamp is null;
update sentilo_orders set event_timestamp = published_at where event_timestamp is null;