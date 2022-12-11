-- LUA script to upgrade how alerts are stored in Redis.  

local alerts = redis.call('KEYS', 'alert:*:aid');

for _,alert in ipairs(alerts) do
    local aid = redis.call('GET', alert);
    local aidKey = 'aid:'..aid;
    local alertId = redis.call('GET', alert);
    redis.call('DEL', aidKey);
    redis.call('HMSET',aidKey,'alert',alertId);        
end