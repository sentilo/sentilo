-- LUA script to change how notification parameters are stored into Redis

local subscriptions = redis.call('KEYS', 'subs:*');

for _,subscription in ipairs(subscriptions) do
    local channels = redis.call('HKEYS', subscription);
        for _,channel in ipairs(channels) do
            local params = redis.call('HGET', subscription, channel);
            if string.find(params, "#@#") == nil and string.find(params, "{") == nil then
                local new_params = '{"endpoint":"'..params..'", "maxRetries": 3, "retryDelay":5}';
                redis.call('HSET', subscription, channel, new_params);
            elseif string.find(params, "#@#") ~= nil then                
				local t = params:find("#@#");
				local endpoint = params:sub(0,t-1);
				local secret = params:sub(t+3);
				local new_params = '{"endpoint":"'..endpoint..'","secretCallbackKey":"'..secret..'", "maxRetries": 3, "retryDelay":5}';
                redis.call('HSET', subscription, channel, new_params);
           end
        end
end