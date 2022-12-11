--[[
 Redis LUA script of sliding window pattern
   accountKey: identifies key that holds account total requests in current window
   now: instant of current request
   window: wdth time of rate limiter window (in ms)
   limit: maximum of allowed requests in current window   
   expireAt: timeout of key accountKey (== window//1000 because is in seconds)
 
 Returns account active state (1 enabled / 0 disabled), total number of requests and score of oldest entry (return zero if account is no enabled)  
]]--

local accountKey = KEYS[1]
local now = tonumber(ARGV[1])
local window = tonumber(ARGV[2])
local limit = tonumber(ARGV[3])

-- remove entries with score older than now - window
local clearBefore = now - window
redis.call('ZREMRANGEBYSCORE', accountKey, 0, clearBefore)

local oldest = "0"
local enabled = 1
local amount = redis.call('ZCARD', accountKey)
if amount < limit then
    local expireAt = math.floor(window/1000)
    redis.call('ZADD', accountKey, now, now)
    redis.call('EXPIRE', accountKey, expireAt)  
    amount = amount + 1  
else     
    local aux = redis.call('ZRANGE',accountKey,0,0)
    oldest = aux[1]
    enabled = 0    
end


return {enabled, amount, tonumber(oldest)}