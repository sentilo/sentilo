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
package org.sentilo.web.catalog.service;

import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.PlatformConfigMessage;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.platform.client.core.domain.Subscription;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;

public interface PlatformService {

  PlatformStatsMessage getCurrentPlatformStats();

  PlatformMetricsMessage getPlatformActivity();

  PlatformMetricsMessage getPlatformPerformance();

  void saveResources(final PlatformAdminInputMessage message);

  void deleteResources(final PlatformAdminInputMessage message);

  List<Subscription> getActiveSubscriptions(final String entity);

  boolean isPlatformRunning();

  PlatformConfigMessage getPlatformConfig();

  void saveCatalogConfig(final Map<String, Map<String, Object>> catalogConfig);

  void saveCatalogMetrics(final SentiloArtifactMetrics catalogMetrics);

  /**
   * Returns platform configuration parameter 'redis.expire.data.seconds' value in minutes.
   */
  int getPlatformTtl();

  String getMetrics();

}
