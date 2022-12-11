-- LUA script to rename existing alert keys from alarm:<alertId>:aid to alert:<alertId>:aid, 
-- and from aid:<aid>:messages to aid:<aid>:alarms.  


local alert_old_keys = redis.call('KEYS', 'alarm:*:aid');

for _,key in ipairs(alert_old_keys) do
    local new_key = string.gsub(key, "alarm:", "alert:");
    redis.call('RENAME',key, new_key);
end

local alarms_old_keys = redis.call('KEYS', 'aid:*:messages');

for _,key in ipairs(alarms_old_keys) do
    local new_key = string.gsub(key, ":messages", ":alarms");
    redis.call('RENAME',key, new_key);
end