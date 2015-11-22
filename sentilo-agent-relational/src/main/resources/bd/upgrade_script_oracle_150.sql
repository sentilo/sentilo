/** Add new column PUBLISHED_AT to the 3 tables*/
ALTER TABLE sentilo_observations ADD (published_at DATE);
ALTER TABLE sentilo_orders ADD (published_at DATE);
ALTER TABLE sentilo_alarms ADD (published_at DATE);