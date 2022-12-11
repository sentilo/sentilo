--[[
 Redis LUA script for token bucket pattern: creates bucket if need be and decrements available tokens by one.
   accountKey: identifies key of bucket that holds account's available tokens   
   limit: maximum of allowed requests in current bucket   
   expireAt: ttl of current bucket (default is 3600, 1h in seconds)       
   
 Return initial quota, remaining number of tokens in bucket and remaining time, in seconds, of current bucket  
]]--

local accountKey = KEYS[1]
local limit = tonumber(ARGV[1])


local exists = redis.call('EXISTS',accountKey)

if exists == 0 then
   redis.call('HSET',accountKey,'quota',limit,'available',limit)
   redis.call('EXPIRE',accountKey,3600)
end

local available = redis.call('HGET',accountKey,'available')

if tonumber(available) > 0 then
   available = redis.call('HINCRBY',accountKey,'available',-1)      
end

local quota = redis.call('HGET',accountKey,'quota')
local expiresAt = redis.call('TTL',accountKey)

return {tonumber(quota), tonumber(available) ,expiresAt}