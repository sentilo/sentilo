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
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.RateLimiterStatus;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import net.jodah.expiringmap.ExpirationListener;
import net.jodah.expiringmap.ExpirationPolicy;
import net.jodah.expiringmap.ExpiringMap;

/**
 * Implementation of the Sliding Window Counter algorithm, which stores counters on memory. Each
 * account represents an entity.
 */
@Component("inboundRateLimiting")
public class InMemorySlidingWindowServiceImpl implements RateLimiterService {

  private static final String KEY_SEPARATOR = ":";

  @Value("${api.global_rate_limit.quota:0}")
  private long globalQuota;

  private Map<String, ConcurrentMap<String, AccountBucket>> windowsAccountsBuckets = new HashMap<String, ConcurrentMap<String, AccountBucket>>();
  private Map<String, WindowAccountInfo> windowsCounters = new HashMap<String, WindowAccountInfo>();
  private Lock lock = new ReentrantLock();

  /**
   * Bucket size time in seconds: divide window account in small buckets of this size (in time). For
   * example, for a window of 1-hour buckets would be of 1-minute While more small buckets are, more
   * precise will be the process but more resources will be needed (more buckets --> more memory and
   * more clean-up process)
   */
  // private static final int PRECISSION = 60; By default, bucket's size time is hard-coded to be 60
  // seconds
  /**
   * TTL of each bucket: pass this time, bucket will be removed. Must be equals to window time -
   * bucket time
   */
  private static final int DURATION = 59 * 60; // 59 minutes

  @Override
  public boolean allow(final String account) {
    final long accountLimit = getAccountQuota(account);
    boolean allow = true;

    if (accountLimit > 0) {
      lock.lock();
      try {
        allow = !overLimitWhenIncremented(account, accountLimit);

        // Increment count into current bucket only if no over limit
        if (allow) {
          initCountersIfNeedBe(account, accountLimit);
          incrementWindowBucketCount(account);
        }
      } finally {
        final QuotaContext qContext =
            new QuotaContext(account, accountLimit, getWindowAccountCount(account), allow ? 0 : getAccountWaitingTime(account));
        QuotaContextHolder.setContext(qContext);
        lock.unlock();
      }
    }
    return allow;
  }

  /**
   * Returns a collection with the current state of each windowAccount.
   * 
   * @return
   */
  public RateLimiterStatus getAccountsStatus() {
    final Set<WindowAccountInfo> accountsCountersCopy = new HashSet<WindowAccountInfo>(windowsCounters.values());
    final RateLimiterStatus rlState = new RateLimiterStatus();

    accountsCountersCopy.forEach(entry -> {
      if (entry.hasBuckets()) {
        final Map<String, Object> accountState = new HashMap<String, Object>();
        accountState.put("quota", entry.getQuota());
        accountState.put("total", entry.getTotal());
        accountState.put("from", entry.getOldestBucket().getCreatedAt().toString());
        accountState.put("to", entry.getOldestBucket().getCreatedAt().plusHours(1).toString());
        rlState.addAccountStatus(entry.getAccount(), accountState);
      }
    });

    return rlState;
  }

  public boolean overLimitWhenIncremented(final String account, final long limit) {
    return limit > 0 && getWindowAccountCount(account) + 1 > limit;
  }

  /**
   * The remaining window time before the rate limit accepts new requests (in minutes). This time is
   * always equals to time remaining to expire the first bucket
   */
  private long getAccountWaitingTime(final String account) {
    // As each bucket has a TTL of 1-hour, to calculate waiting time, the following steps should be
    // evaluated
    // - Get oldest bucket from account window. It has associate a creation date
    // - If account is blocked, it means current_time < oldest bucket expired time =
    // bucket_creation_date + 1 hour,
    // or what is the same, bucket_creation_date < current_time < bucket_creation_date+1 hour
    // Account window will allow new requests when oldest bucket expires, it is after
    // (bucket_creation_date + 1 hour - current_time).

    final ZonedDateTime now = ZonedDateTime.now();
    final ZonedDateTime oldestBucketTime = windowsCounters.get(account).getOldestBucket().getCreatedAt();

    return ChronoUnit.MINUTES.between(now, oldestBucketTime.plusHours(1));
  }

  /**
   * Returns the number of requests allowed in an hour (window size) to this account.
   * 
   * @param account
   * @return quota
   */
  private long getAccountQuota(final String account) {
    return account.equals(RateLimiterService.INSTANCE_ID) ? globalQuota : RequesterContextHolder.getContext().getMetadata().getApiInputQuota();
  }

  private long getWindowAccountCount(final String account) {
    return windowsCounters.containsKey(account) ? windowsCounters.get(account).getTotal() : 0;
  }

  /**
   * Increments both current window bucket counter and window global counter by 1
   * 
   * @param token Account identifier.
   */
  private void incrementWindowBucketCount(final String account) {
    final AccountBucket current_account_bucket = getCurrentBucket(account);

    // First, add current bucket to account if it isn't already and to global window account info
    if (!windowsAccountsBuckets.get(account).containsKey(current_account_bucket.getLabel())) {
      windowsAccountsBuckets.get(account).put(current_account_bucket.getLabel(), current_account_bucket);
      windowsCounters.get(account).addBucket(current_account_bucket);
    }

    // Next, increment by 1 current bucket counter
    windowsAccountsBuckets.get(account).get(current_account_bucket.getLabel()).incrementAndGet();

    // Finally, increment by 1 global window counter
    windowsCounters.get(account).increment(1);
  }

  private void initCountersIfNeedBe(final String account, final long quota) {
    // Define new window bucket associated with this account if its'n exist
    if (!windowsAccountsBuckets.containsKey(account)) {
      windowsAccountsBuckets.put(account, buildWindowBucket());
    }

    // Define new window counter associated with this account if its'n exist, or update stored quota
    // if it has changed
    if (!windowsCounters.containsKey(account)) {
      windowsCounters.put(account, new WindowAccountInfo(account, quota));
    } else {
      windowsCounters.get(account).updateQuota(quota);
    }
  }

  private ConcurrentMap<String, AccountBucket> buildWindowBucket() {
    final ConcurrentMap<String, AccountBucket> windowBuckets = ExpiringMap.builder().expiration(DURATION, TimeUnit.SECONDS)
        .expirationPolicy(ExpirationPolicy.CREATED).expirationListener(new ExpirationListener<String, AccountBucket>() {

          public void expired(final String bucketKey, final AccountBucket bucket) {
            final String account = bucketKey.split(KEY_SEPARATOR)[0];
            windowsCounters.get(account).removeBucket(bucketKey, bucket.getTotal());
            if (!windowsCounters.get(account).hasBuckets()) {
              windowsCounters.remove(account);
            }
          }
        }).build();

    return windowBuckets;
  }

  /**
   * Returns the bucket associated with the current instant. As each window is divided into 60 small
   * buckets (each one with a length of 1-minute), each bucket is labeled following the next
   * expression: <account>:<current_instant_hour>:<bucket_number> where bucket_number goes from 0 to
   * 59 (i.e. the current minute)
   * 
   * @param account Account identifier
   * @return Current bucket for this token
   */
  private AccountBucket getCurrentBucket(final String account) {
    final ZonedDateTime zdt = ZonedDateTime.now();
    return new AccountBucket(zdt, account + KEY_SEPARATOR + zdt.getHour() + KEY_SEPARATOR + zdt.getMinute());
  }

  class AccountBucket {

    private ZonedDateTime createdAt;
    private String label;
    private long total;

    public AccountBucket(final ZonedDateTime createdAt, final String label) {
      this.createdAt = createdAt;
      this.label = label;
      total = 0l;
    }

    public ZonedDateTime getCreatedAt() {
      return createdAt;
    }

    public String getLabel() {
      return label;
    }

    public long getTotal() {
      return total;
    }

    public long incrementAndGet() {
      return ++total;
    }
  }

  class WindowAccountInfo {

    private String account;
    private long total;
    private long quota;
    private List<AccountBucket> orderedBuckets;

    public WindowAccountInfo() {
      total = 0l;
      orderedBuckets = new ArrayList<AccountBucket>();
    }

    public WindowAccountInfo(final String account, final long quota) {
      this();
      this.account = account;
      this.quota = quota;
    }

    public String printState() {
      return "";
    }

    public long getTotal() {
      return total;
    }

    public long getQuota() {
      return quota;
    }

    public String getAccount() {
      return account;
    }

    public void updateQuota(final long newQuota) {
      if (quota != newQuota) {
        quota = newQuota;
      }
    }

    public void increment(final long quantity) {
      total += quantity;
    }

    public boolean hasBuckets() {
      return !orderedBuckets.isEmpty();
    }

    public void addBucket(final AccountBucket bucket) {
      orderedBuckets.add(bucket);
    }

    public void removeBucket(final String bucketLabel, final long bucketCount) {
      // buckets are ordered by creation time, so they expired in order too (orderedBuckets is a
      // FIFO collection)
      orderedBuckets.remove(orderedBuckets.size() - 1);
      // orderedBuckets.remove(bucketLabel);
      total -= bucketCount;
    }

    public AccountBucket getOldestBucket() {
      return orderedBuckets.get(0);
    }
  }
}
