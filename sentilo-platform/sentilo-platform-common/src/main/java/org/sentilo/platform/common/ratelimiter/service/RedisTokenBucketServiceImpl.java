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

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
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
import org.springframework.scripting.support.ResourceScriptSource;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component("outboundRateLimiting")
@Profile("cluster")
@SuppressWarnings("rawtypes")
public class RedisTokenBucketServiceImpl extends AbstractTokenBucketServiceImpl implements TokenBucketService {

  private static final Logger LOGGER = LoggerFactory.getLogger(RedisTokenBucketServiceImpl.class);

  private final static long ONE_HOUR_SECONDS = 60 * 60l;
  private final String accountKeyTmpl = "rl:out:account:%s";
  private final String accountsKey = "rl:out:accounts";

  @Autowired
  private StringRedisTemplate redisTemplate;

  private DefaultRedisScript<List> tokenBucketScript;
  private DefaultRedisScript<List> tokenBucketStatusScript;

  private Lock lock;

  @PostConstruct
  public void init() {
    lock = lockProvider.getLock(lockName);
  }

  @Override
  public boolean allow(final String account) {
    final long accountLimit = getAccountQuota(account);
    boolean allow = true;
    Tuple3<Long, Long, Long> response = Tuple3.of(accountLimit, 0l, ONE_HOUR_SECONDS);
    if (accountLimit > 0) {
      try {
        response = requestsAllowed(account, accountLimit);
        allow = response.getSecond() > 0;
      } finally {
        final long tokensConsumed = response.getFirst() - response.getSecond();
        final QuotaContext qContext = new QuotaContext(account, accountLimit, tokensConsumed, minutesToReset(response.getThird()));
        QuotaContextHolder.setContext(qContext);
      }
    }
    return allow;
  }

  /**
   * Returns a collection with the current state of each accountBucket.
   *
   * @return
   */
  @Override
  public RateLimiterStatus getAccountsStatus() {
    final RateLimiterStatus rlState = new RateLimiterStatus();

    final Map<Object, Object> accounts = redisTemplate.opsForHash().entries(accountsKey);
    final List<String> expiredAccounts = new ArrayList<String>();
    accounts.forEach((key, value) ->
      {
        final String account = (String) key;
        final String accountKey = buildKeyAccount(account);
        if (redisTemplate.hasKey(accountKey)) {
          final Tuple3<Long, Long, Long> accountStatus = accountStatus(accountKey);
          final long quota = accountStatus.getFirst();
          final long available = accountStatus.getSecond();
          final long expiresAt = accountStatus.getThird(); // remaining time to live of bucket in
                                                           // seconds
          final ZonedDateTime from = fromBucketTime(expiresAt);
          final long total = quota - available;
          rlState.addAccountStatus(account, buildAccountState(quota, total, from));
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

  protected long minutesToReset(final long secondsToReset) {
    return TimeUnit.SECONDS.toMinutes(secondsToReset);
  }

  private Tuple3<Long, Long, Long> requestsAllowed(final String account, final long limit) {
    long available = 0L;
    long ttl = ONE_HOUR_SECONDS;
    long quota = limit;
    try {
      final List response = executeScript(account, limit);
      if (!CollectionUtils.isEmpty(response)) {
        quota = (long) response.get(0);
        available = (long) response.get(1);
        ttl = (long) response.get(2);
        // Finally, add account to the register of active accounts
        redisTemplate.opsForHash().put(accountsKey, account, Long.toString(quota));
      }
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while running rate limiter process: {}", e);
    }

    return Tuple3.of(quota, available, ttl);
  }

  private List executeScript(final String account, final long limit) {
    List<?> response = new ArrayList<>();
    try {
      lock.lock();
      final String accountKey = buildKeyAccount(account);
      response = redisTemplate.execute(getTokenBucketScript(), Collections.singletonList(accountKey), Long.toString(limit));
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while running rate limiter process: {}", e);
    } finally {
      lock.unlock();
    }

    return response;
  }

  private String buildKeyAccount(final String account) {
    return String.format(accountKeyTmpl, account);
  }

  private ZonedDateTime fromBucketTime(final long bucketExpiresAtInSeconds) {
    // Bucket init TTL is 3600 seconds (== 1h).
    // --> bucketExpiresAtInSeconds == remaining time to live in seconds of bucket
    // --> fromBucket + 1h_in_seconds = now + bucketExpiresAtInSeconds
    // --> fromBucket = now + (bucketExpiresAtInSeconds - 3600)
    final ZonedDateTime now = ZonedDateTime.now();
    final long secondsToSubstract = 3600 - bucketExpiresAtInSeconds;
    return now.minusSeconds(secondsToSubstract);
  }

  private Tuple3<Long, Long, Long> accountStatus(final String accountKey) {
    long quota = -1L;
    long available = -1L;
    long expiresAt = -1L;

    try {
      final List response = redisTemplate.execute(getTokenBucketStatusScript(), Collections.singletonList(accountKey));
      quota = (long) response.get(0);
      available = (long) response.get(1);
      expiresAt = (long) response.get(2);
    } catch (final Exception e) {
      // Nothing to do. Return default values
      LOGGER.warn("An error has been raised while reading account status for account {}. Returning default values", accountKey, e);
    }

    return Tuple3.of(quota, available, expiresAt);
  }

  private RedisScript<List> getTokenBucketScript() {
    if (tokenBucketScript == null) {
      tokenBucketScript = new DefaultRedisScript<List>();
      tokenBucketScript.setScriptSource(new ResourceScriptSource(new ClassPathResource("scripts/rl_token_bucket.lua")));
      tokenBucketScript.setResultType(List.class);
    }

    return tokenBucketScript;
  }

  private RedisScript<List> getTokenBucketStatusScript() {
    if (tokenBucketStatusScript == null) {
      tokenBucketStatusScript = new DefaultRedisScript<List>();
      tokenBucketStatusScript.setScriptSource(new ResourceScriptSource(new ClassPathResource("scripts/rl_token_bucket_status.lua")));
      tokenBucketStatusScript.setResultType(List.class);
    }

    return tokenBucketStatusScript;
  }

}
