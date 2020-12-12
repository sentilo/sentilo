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
package org.sentilo.web.catalog.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.PlatformConfigMessage;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PlatformServiceImpl implements PlatformService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PlatformServiceImpl.class);

  @Autowired
  private RESTClient restClient;

  /**
   * Internal cache to evict continues calls to config API service. Cache is initialized with an
   * expired time of 30 minutes
   */
  private LRUCache<String, Object> platformConfigParams = new LRUCacheImpl<String, Object>(100, 30);

  private final static String DEFAULT_PLATFORM_TTL = "DEFAULT_PLATFORM_TTL";

  private final StringMessageConverter parser = new DefaultStringMessageConverter();

  @Override
  public PlatformStatsMessage getCurrentPlatformStats() {
    final RequestContext rc = new RequestContext("admin/stats");
    final String response = restClient.get(rc);
    return (PlatformStatsMessage) parser.unmarshal(response, PlatformStatsMessage.class);
  }

  @Override
  public PlatformMetricsMessage getPlatformActivity() {
    final RequestContext rc = new RequestContext("admin/activity");
    final String response = restClient.get(rc);
    return (PlatformMetricsMessage) parser.unmarshal(response, PlatformMetricsMessage.class);
  }

  @Override
  public PlatformMetricsMessage getPlatformPerformance() {
    final RequestContext rc = new RequestContext("admin/performance");
    final String response = restClient.get(rc);
    return (PlatformMetricsMessage) parser.unmarshal(response, PlatformMetricsMessage.class);
  }

  @Override
  public PlatformConfigMessage getPlatformConfig() {
    final RequestContext rc = new RequestContext("admin/config");
    final String response = restClient.get(rc);
    return (PlatformConfigMessage) parser.unmarshal(response, PlatformConfigMessage.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.PlatformService#saveResources(org.sentilo.web.catalog.domain.
   * PlatformAdminInputMessage)
   */
  @Override
  public void saveResources(final PlatformAdminInputMessage message) {
    final RequestContext rc = new RequestContext("admin/save", parser.marshal(message));
    restClient.post(rc);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.PlatformService#deleteResources(org.sentilo.web.catalog.domain.
   * PlatformAdminInputMessage)
   */
  @Override
  public void deleteResources(final PlatformAdminInputMessage message) {
    final RequestContext rc = new RequestContext("admin/delete", parser.marshal(message));
    restClient.put(rc);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#getActiveSubscriptions(java.lang.String)
   */
  @Override
  public List<Subscription> getActiveSubscriptions(final String entity) {
    final String path = String.format("admin/subscriptions/%s", entity);
    final RequestContext rc = new RequestContext(path);
    final String response = restClient.get(rc);
    final SubscriptionsOutputMessage som = (SubscriptionsOutputMessage) parser.unmarshal(response, SubscriptionsOutputMessage.class);
    return som.getSubscriptions();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#isPlatformRunning()
   */
  @Override
  public boolean isPlatformRunning() {
    boolean isRunning = true;

    try {
      final RequestContext rc = new RequestContext("admin/ping");
      // rc.setIdentityToken("******");
      restClient.get(rc);
    } catch (final RESTClientException rce) {
      //
      // Expected error is an Invalid token, i.e. status = 401
      // isRunning = rce.getStatus() == 401 ? true : false;
      isRunning = false;
    }

    return isRunning;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#getPlatformTtl()
   */
  @Override
  public int getPlatformTtl() {
    Object oPlatformTtlConfigInMinutes = platformConfigParams.get(DEFAULT_PLATFORM_TTL);
    if (oPlatformTtlConfigInMinutes == null) {
      oPlatformTtlConfigInMinutes = getAndConvertDefaultPlatformTtl();
    }

    return (Integer) oPlatformTtlConfigInMinutes;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#saveCatalogConfig(java.util.Map)
   */
  @Override
  public void saveCatalogConfig(final Map<String, Map<String, Object>> catalogConfig) {
    if (isPlatformRunning()) {
      final PlatformAdminInputMessage message = new PlatformAdminInputMessage();
      message.setArtifactsConfig(catalogConfig);

      final RequestContext rc = new RequestContext("admin/config", parser.marshal(message));
      restClient.post(rc);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.PlatformService#saveCatalogMetrics(org.sentilo.common.metrics.
   * SentiloArtifactMetrics)
   */
  @Override
  public void saveCatalogMetrics(final SentiloArtifactMetrics catalogMetrics) {
    if (isPlatformRunning()) {
      final PlatformAdminInputMessage message = new PlatformAdminInputMessage();
      message.setArtifactsMetrics(Arrays.asList(catalogMetrics));

      final RequestContext rc = new RequestContext("admin/metrics", parser.marshal(message));
      restClient.post(rc);
    }
  }

  private int getAndConvertDefaultPlatformTtl() {
    final String platformTllConfigName = Constants.PLATFORM_DEFAULT_TTL_KEY;
    int platformTtlConfigInMinutes = -1;

    try {
      final PlatformConfigMessage platformConfig = getPlatformConfig();
      for (final String key : platformConfig.getArtifactsConfig().keySet()) {
        if (platformConfig.getArtifactsConfig().get(key).containsKey(platformTllConfigName)) {
          final String redisExpireDataSeconds = (String) platformConfig.getArtifactsConfig().get(key).get(platformTllConfigName);
          platformTtlConfigInMinutes = Integer.parseInt(redisExpireDataSeconds) / 60;
          platformConfigParams.put(DEFAULT_PLATFORM_TTL, platformTtlConfigInMinutes);
          break;
        }
      }
    } catch (final Exception e) {
      LOGGER.warn("An error has raised while reading platform config parameters.", e);
    }

    return platformTtlConfigInMinutes;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PlatformService#getMetrics()
   */
  @Override
  public String getMetrics() {
    final RequestContext rc = new RequestContext("admin/metrics");
    return restClient.get(rc);
  }
}
