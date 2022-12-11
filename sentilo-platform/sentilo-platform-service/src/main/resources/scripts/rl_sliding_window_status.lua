--[[
 Redis LUA script to check status of an account
   accountKey: identifies account's key
   now: instant of current request
   window: wdth time of rate limiter window (in ms)   
       
 Returns number of requests in current window and score of oldest entry  
]]--

local accountKey = KEYS[1]
local now = tonumber(ARGV[1])
local window = tonumber(ARGV[2])

local clearBefore = now - window
redis.call('ZREMRANGEBYSCORE', accountKey, 0, clearBefore)

local oldest = "0"
local amount = redis.call('ZCARD', accountKey)
if amount > 0 then
    local aux = redis.call('ZRANGE',accountKey,0,0)
    oldest = aux[1]        
end

return {amount, tonumber(oldest)}