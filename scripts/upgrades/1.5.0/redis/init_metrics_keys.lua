-- LUA script to initialize metrics keys from current event counters
-- request counter has an approximate initial value because it is impossible to know how many requests 
-- has  be done from the beginning.

local maxAvg = redis.call('GET', 'stats:avg:max');
local totalOrders = redis.call('GET', 'global:soid');
local totalObs = redis.call('GET', 'global:sdid');
local totalAlarms = redis.call('GET', 'global:amid');

local nTotalOrders = tonumber(totalOrders) or 0;
local nTotalObs = tonumber(totalObs) or 0;
local nTotalAlarms = tonumber(totalAlarms) or 0;

print("Orders ",totalOrders);
print("Alarms: ",totalAlarms);
print("Obs: ",totalObs);

local requests = nTotalOrders + nTotalObs + nTotalAlarms;
local sRequests = tostring(requests);

redis.call('HSET','avgs:master','requests',sRequests);
redis.call('HSET','avgs:master','maxAvg',maxAvg);

redis.call('HSET','counters:master','requests',sRequests);
redis.call('HSET','counters:master','data_put',tostring(nTotalObs));
redis.call('HSET','counters:master','order_put',tostring(nTotalOrders));
redis.call('HSET','counters:master','alarm_put',tostring(nTotalAlarms));