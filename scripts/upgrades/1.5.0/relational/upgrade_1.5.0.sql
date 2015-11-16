/**
 * Add a new timestamp column, named published_at, to data tables to accelerate filtering queries by date.   
 */

-- For MySQL
-- ALTER TABLE sentilo_observations ADD published_at TIMESTAMP NULL;
-- ALTER TABLE sentilo_orders ADD published_at TIMESTAMP NULL;
-- ALTER TABLE sentilo_alarms ADD published_at TIMESTAMP NULL;

-- For Oracle
-- ALTER TABLE sentilo_observations ADD (published_at DATE);
-- ALTER TABLE sentilo_orders ADD (published_at DATE);
-- ALTER TABLE sentilo_alarms ADD (published_at DATE);

/* To set the value of this column on the previous rows, you could execute:  */
-- For Oracle (uncomment)
-- update sentilo_observations set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;
-- update sentilo_alarms set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;
-- update sentilo_orders set published_at = TO_DATE(timestamp, 'dd/mm/YYYY"T"HH24:MI:SS') where published_at is null;

-- For MySQL (uncomment)
-- update sentilo_observations set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;
-- update sentilo_alarms set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;
-- update sentilo_orders set published_at = STR_TO_DATE(timestamp, '%d/%m/%Y"T"%H:%i:%s') where published_at is null;