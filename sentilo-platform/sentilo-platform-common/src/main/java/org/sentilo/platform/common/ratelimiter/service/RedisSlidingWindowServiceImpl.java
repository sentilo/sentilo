/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.common.ratelimiter.service;

import java.time.Clock;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;

import javax.annotation.PostConstruct;

import org.sentilo.common.utils.Tuple3;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.data.util.Pair;
import org.springframework.scripting.support.ResourceScriptSource;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * Implementation of the Sliding Window Counter algorithm, which stores counters on Redis. Each
 * account represents an entity.
 */
@Component("inboundRateLimiting")
@Profile("cluster")
@SuppressWarnings("rawtypes")
public class RedisSlidingWindowServiceImpl extends AbstractSlidingWindowServiceImpl implements SlidingWindowService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RedisSlidingWindowServiceImpl.class);

  @Autowired
  private StringRedisTemplate redisTemplate;

  private DefaultRedisScript<List> slidingWindowScript;
  private DefaultRedisScript<List> slidingWindowStatusScript;

  private final String accountKeyTmpl = "rl:in:account:%s";
  private final String accountsKey = "rl:in:accounts";

  private Lock lock;

  @PostConstruct
  public void init() {
    lock = lockProvider.getLock(lockName);
  }

  @Override
  public boolean allow(final String account) {
    final long accountLimit = getAccountQuota(account);
    boolean allow = true;
    if (accountLimit > 0) {
      Tuple3<Boolean, Long, Long> response = Tuple3.of(false, -1l, -1l);
      lock.lock();
      try {
        response = requestsAllowed(account, WINDOW_SIZE.toMillis(), accountLimit);
        allow = response.getFirst();
      } finally {
        lock.unlock();
        final long blockMinutesTime = allow ? 0 : minutesToReset(response.getThird());
        final QuotaContext qContext = new QuotaContext(account, accountLimit, response.getSecond(), blockMinutesTime);
        QuotaContextHolder.setContext(qContext);
      }
    }
    return allow;
  }

  @Override
  public RateLimiterStatus getAccountsStatus() {
    final RateLimiterStatus rlState = new RateLimiterStatus();

    final Map<Object, Object> accounts = redisTemplate.opsForHash().entries(accountsKey);
    final List<String> expiredAccounts = new ArrayList<String>();
    accounts.forEach((k, v) ->
      {
        final String account = (String) k;
        final String accountKey = buildKeyAccount(account);
        final Long value = Long.parseLong((String) v);
        if (redisTemplate.hasKey(accountKey)) {
          final Pair<Long, Long> accountStatus = accountStatus(accountKey, WINDOW_SIZE.toMillis());
          final long total = accountStatus.getFirst();
          final long oldest = accountStatus.getSecond();
          final ZonedDateTime oldestZdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(oldest), Clock.systemDefaultZone().getZone());
          rlState.addAccountStatus(account, buildAccountState(value, total, oldestZdt));
        } else {
          expiredAccounts.add(account);
        }
      });

    // Remove all expired entries from accountsKey
    if (!CollectionUtils.isEmpty(expiredAccounts)) {
      redisTemplate.opsForHash().delete(accountsKey, expiredAccounts.toArray());
    }
    return rlState;

  }

  private long minutesToReset(final long oldestEntry) {
    return minutesToReset(Instant.ofEpochMilli(oldestEntry));
  }

  private String buildKeyAccount(final String account) {
    return String.format(accountKeyTmpl, account);
  }

  private Tuple3<Boolean, Long, Long> requestsAllowed(final String account, final long window, final long limit) {
    long count = -1L;
    long oldestTime = -1L;
    boolean active = true;
    try {
      final List response = executeScript(account, window, limit);
      if (!CollectionUtils.isEmpty(response)) {
        active = (long) response.get(0) == 1;
        count = (long) response.get(1);
        // time in millis of the oldest request in current window
        oldestTime = (long) response.get(2);
        // Finally, add account to the register of active accounts
        redisTemplate.opsForHash().put(accountsKey, account, Long.toString(limit));
      }
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while running rate limiter process: {}", e.getMessage(), e);
    }

    return Tuple3.of(active, count, oldestTime);
  }

  private List executeScript(final String account, final long window, final long limit) {
    List<?> response = new ArrayList<>();
    try {
      lock.lock();
      final String accountKey = buildKeyAccount(account);
      final String now = Long.toString(Instant.now().toEpochMilli());
      response =
          redisTemplate.execute(getSlidingWindowScript(), Collections.singletonList(accountKey), now, Long.toString(window), Long.toString(limit));
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while running rate limiter process: {}", e);
    } finally {
      lock.unlock();
    }

    return response;
  }

  private Pair<Long, Long> accountStatus(final String accountKey, final long window) {
    long total = 0L;
    long oldestTime = -1L;

    try {
      final String now = Long.toString(Instant.now().toEpochMilli());
      final List response = redisTemplate.execute(getSlidingWindowStatusScript(), Collections.singletonList(accountKey), now, Long.toString(window));
      total = (long) response.get(0);
      // time in millis of the oldest request in current window
      oldestTime = (long) response.get(1);
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while running rate limiter process: {}", e.getMessage(), e);
    }

    return Pair.of(total, oldestTime);
  }

  private RedisScript<List> getSlidingWindowScript() {
    if (slidingWindowScript == null) {
      slidingWindowScript = new DefaultRedisScript<List>();
      slidingWindowScript.setScriptSource(new ResourceScriptSource(new ClassPathResource("scripts/rl_sliding_window.lua")));
      slidingWindowScript.setResultType(List.class);
    }

    return slidingWindowScript;
  }

  private RedisScript<List> getSlidingWindowStatusScript() {
    if (slidingWindowStatusScript == null) {
      slidingWindowStatusScript = new DefaultRedisScript<List>();
      slidingWindowStatusScript.setScriptSource(new ResourceScriptSource(new ClassPathResource("scripts/rl_sliding_window_status.lua")));
      slidingWindowStatusScript.setResultType(List.class);
    }

    return slidingWindowStatusScript;
  }
}
