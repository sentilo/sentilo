--[[
 Redis LUA script to check status of a token bucket account.
   accountKey: identifies key of bucket that holds account's available tokens              
   
 If account exists, returns initial quota, remaining number of tokens in bucket and remaining time, in seconds, of current bucket  
]]--

local accountKey = KEYS[1]

local quota = -1
local available = -1
local expiresAt = -1

local exists = redis.call('EXISTS',accountKey)

if exists == 1 then
	quota = redis.call('HGET',accountKey,'quota')
	available = redis.call('HGET',accountKey,'available')	
	expiresAt = redis.call('TTL',accountKey)   
end

return {tonumber(quota), tonumber(available),expiresAt}