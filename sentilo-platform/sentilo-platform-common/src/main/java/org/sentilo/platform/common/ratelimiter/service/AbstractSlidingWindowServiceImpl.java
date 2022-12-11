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

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;

import org.sentilo.platform.common.security.RequesterContextHolder;
import org.springframework.beans.factory.annotation.Value;

public abstract class AbstractSlidingWindowServiceImpl extends AbstractRateLimiterServiceImpl {

  @Value("${sentilo.server.api.global_rate_limit.quota:0}")
  protected long globalQuota;

  protected final String lockName = "rl_sliding_window";

  protected final Duration WINDOW_SIZE = Duration.ofHours(1);

  /**
   * Returns the number of requests allowed in an hour (window size) to this account.
   *
   * @param account
   * @return quota
   */
  protected long getAccountQuota(final String account) {
    return account.equals(RateLimiterService.INSTANCE_ID) ? globalQuota : RequesterContextHolder.getContext().getMetadata().getApiInputQuota();
  }

  /**
   * As each bucket (or account) has associated a sliding window of size 1-hour, to calculate
   * waiting time when no more requests are allowed, i.e., when account window will allow new
   * requests, is necessary to calculate when oldest bucket expires, and it is after:
   *
   * oldestEntry + 1 hour - current_time
   */
  protected long minutesToReset(final Temporal oldestEntry) {
    final Instant now = Instant.now();
    return ChronoUnit.MINUTES.between(now, oldestEntry.plus(1, ChronoUnit.HOURS));
  }
}
