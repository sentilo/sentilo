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
package org.sentilo.platform.server.request.interceptor;

import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.exception.InboundRateLimiterException;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.server.request.SentiloRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class RateLimiterInterceptor implements SentiloRequestHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(RateLimiterInterceptor.class);

  @Autowired
  @Qualifier("inboundRateLimiting")
  private RateLimiterService rateLimiterService;

  @Autowired
  private InternalAlarmService internalAlarmService;

  @Autowired
  private SentiloArtifactConfigService serviceConfig;

  @Value("${api.global_rate_limit.quota:0}")
  private long globalQuota;

  @Override
  public void invoke(final SentiloRequest request) throws InboundRateLimiterException {

    // If it is enabled, global rate limiter control has priority over account rate limiter
    // The algorithm used by the instance rate limiter will be the same algorithm configured for the
    // account rate limiter
    if (applyGlobalRateLimiter(request) && !rateLimiterService.allow(RateLimiterService.INSTANCE_ID)) {
      LOGGER.warn("Global inbound requests limit, [{}/per hour], exceeded!! ", globalQuota);
      internalAlarmService.publishInboundRateLimiterAlarm(request.getEntitySource(), QuotaContextHolder.getContext(QuotaContext.Type.GLOBAL));
      throw InboundRateLimiterException.buildGlobalInboundRateLimiterException(globalQuota);
    }

    // Check account rate limiter
    if (isAccountRateLimiterEnabled() && !rateLimiterService.allow(request.getEntitySource())) {
      final QuotaContext qc = QuotaContextHolder.getContext(QuotaContext.Type.ENTITY);
      LOGGER.warn("Entity [{}] has exceeded its inbound requests quota [{}/per hour]!", request.getEntitySource(), qc.getLimit());
      internalAlarmService.publishInboundRateLimiterAlarm(request.getEntitySource(), qc);
      throw InboundRateLimiterException.buildAccountInboundRateLimiterException(qc.getLimit());
    }
  }

  // Global rate limiter only applies to foreign inbound requests, i.e., does not apply to requests
  // from other Sentilo's modules
  // These requests are done by entity with id $catalog.id
  private boolean applyGlobalRateLimiter(final SentiloRequest request) {
    return isGlobalRateLimiterEnabled()
        && !request.getEntitySource().equals(serviceConfig.getConfigValue("catalog.id", SentiloConstants.DEFAULT_CATALOG_ID));
  }

  private boolean isGlobalRateLimiterEnabled() {
    return globalQuota > 0;
  }

  private boolean isAccountRateLimiterEnabled() {
    return RequesterContextHolder.getContext().getMetadata().getApiInputQuota() > 0;
  }

}
