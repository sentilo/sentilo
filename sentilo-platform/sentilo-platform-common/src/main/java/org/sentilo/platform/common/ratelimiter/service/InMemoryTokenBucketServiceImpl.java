/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 *
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 *
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 *
 * See the licenses for the specific language governing permissions, limitations and more details.
 *
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 *
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.common.ratelimiter.service;

import java.time.ZonedDateTime;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;

import javax.annotation.PostConstruct;

import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import net.jodah.expiringmap.ExpirationPolicy;
import net.jodah.expiringmap.ExpiringMap;

/**
 * Implementation of the Token Bucket algorithm, where buckets are stored on memory. Buckets are
 * initialized with N tokens (where N depends on each account), and each call to <code>allow</code>
 * method try to take a token from the bucket. If there is no token available in it then request is
 * rejected. Finally, each hour buckets are refilled.
 */
@Component("outboundRateLimiting")
@Profile("!cluster")
public class InMemoryTokenBucketServiceImpl extends AbstractTokenBucketServiceImpl implements TokenBucketService {

  private final static long ONE_HOUR_MS = 60 * 60 * 1000l;

  private ExpiringMap<String, AccountBucket> accountsBuckets;
  private Lock lock;

  @PostConstruct
  public void init() {
    lock = lockProvider.getLock(lockName);

    accountsBuckets =
        ExpiringMap.builder().expiration(ONE_HOUR_MS, TimeUnit.MILLISECONDS).expirationPolicy(ExpirationPolicy.CREATED)
            .expirationListener(
                (final String key, final AccountBucket accountBucket) -> accountsBuckets.put(key, accountBucket.refill(getAccountQuota(key))))
            .build();
  }

  @Override
  public boolean allow(final String account) {
    final String token = account;
    final long accountLimit = getAccountQuota(account);
    boolean allow = true;

    if (accountLimit > 0) {
      lock.lock();
      try {
        initBucketIfNeedBe(token, accountLimit);
        allow = isTokenAvailable(token);
      } finally {
        final long tokensConsumed = accountsBuckets.get(token).consumed();
        final QuotaContext qContext = new QuotaContext(token, accountLimit, tokensConsumed, getAccountWaitingTime(token));
        QuotaContextHolder.setContext(qContext);
        lock.unlock();
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
    final Set<AccountBucket> accountsCountersCopy = new HashSet<AccountBucket>(accountsBuckets.values());
    final RateLimiterStatus rlState = new RateLimiterStatus();

    accountsCountersCopy.forEach(entry ->
      {
        final long total = entry.getQuota() - entry.getAvailable();
        rlState.addAccountStatus(entry.getAccount(), buildAccountState(entry.getQuota(), total, entry.getCreatedAt()));
      });

    return rlState;
  }

  /**
   * Try to get a token from account bucket, and return try/false if it is available
   *
   * @param token Account identifier.
   */
  private boolean isTokenAvailable(final String token) {
    return accountsBuckets.get(token).decrementAndGet() >= 0 ? true : false;
  }

  /**
   * The remaining time before the rate limit accepts new requests (in minutes), i.e time remaining
   * to refill the bucket.
   */
  private long getAccountWaitingTime(final String token) {
    return TimeUnit.MILLISECONDS.toMinutes(accountsBuckets.getExpectedExpiration(token));
  }

  private void initBucketIfNeedBe(final String token, final long quota) {
    // Add bucket associated with this account if its'n exist
    if (!accountsBuckets.containsKey(token)) {
      accountsBuckets.put(token, new AccountBucket(token, quota));
    }
  }

  class AccountBucket {

    private final String account;
    private long quota;
    private long available;
    private ZonedDateTime createdAt;

    public AccountBucket(final String account, final long quota) {
      this.account = account;
      this.createdAt = ZonedDateTime.now();
      init(quota);
    }

    public AccountBucket refill(final long quota) {
      init(quota);
      return this;
    }

    private void init(final long quota) {
      this.quota = quota;
      // Initially, available tokens equals to quota tokens
      available = quota;
    }

    public long getQuota() {
      return quota;
    }

    public long getAvailable() {
      return available <= 0 ? 0 : available;
    }

    public long consumed() {
      return quota - getAvailable();
    }

    public String getAccount() {
      return account;
    }

    public ZonedDateTime getCreatedAt() {
      return createdAt;
    }

    public long decrementAndGet() {
      return --available;
    }

  }
}
