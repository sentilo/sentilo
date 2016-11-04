/** Add new column PUBLISHED_AT to the 3 tables*/
ALTER TABLE sentilo_observations ADD (published_at DATE);
ALTER TABLE sentilo_orders ADD (published_at DATE);
ALTER TABLE sentilo_alarms ADD (published_at DATE);

/* To set the value of this column on the previous rows, you could execute:  */
update sentilo_observations set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;
update sentilo_alarms set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;
update sentilo_orders set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;