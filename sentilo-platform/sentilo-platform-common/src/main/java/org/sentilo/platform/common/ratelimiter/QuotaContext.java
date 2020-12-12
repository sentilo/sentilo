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
package org.sentilo.platform.common.ratelimiter;

import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;

/**
 * Holds current state of account's quota, i.e., quota account state in the current window, where
 * account may be instance or entity (and in a future tenant)
 */
public class QuotaContext {

  public static enum Type {
    GLOBAL, TENANT, ENTITY
  };

  /** Account quota identifier */
  private String account;

  /** Maximum number of tokens disposable per window */
  private long limit;

  /** Number of tokens consumed in the current window */
  private long current;

  /** Time, in minutes, to reinitialize window time */
  private long minutesToReset;

  private Type type;

  public QuotaContext(final String account, final long limit, final long current, final long minutesToReset) {
    this.account = account;
    this.limit = limit;
    this.current = current;
    this.minutesToReset = minutesToReset;
    type = account.equals(RateLimiterService.INSTANCE_ID) ? QuotaContext.Type.GLOBAL : QuotaContext.Type.ENTITY;
  }

  public String getAccount() {
    return account;
  }

  public long getRemaining() {
    return limit - current;
  }

  public long getLimit() {
    return limit;
  }

  public long getCurrent() {
    return current;
  }

  public long getMinutesToReset() {
    return minutesToReset;
  }

  public Type getType() {
    return type;
  }
}
