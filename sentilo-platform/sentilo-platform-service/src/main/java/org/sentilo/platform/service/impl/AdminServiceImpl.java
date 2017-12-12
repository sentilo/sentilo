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
package org.sentilo.platform.service.impl;

import java.util.List;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.monitor.CounterService;
import org.sentilo.platform.service.monitor.MetricService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class AdminServiceImpl extends AbstractPlatformServiceImpl implements AdminService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AdminServiceImpl.class);

  @Autowired
  private SubscribeService subscribeService;

  @Autowired
  private ResourceService resourceService;

  @Autowired
  private CounterService counterService;
  @Autowired
  private MetricService metricService;

  @Override
  public Statistics getStatistics() {
    return null;
  }

  @Override
  public PlatformMetricsMessage getPerformance() {
    final PlatformMetricsMessage metricsMessage = new PlatformMetricsMessage();
    metricsMessage.setPerformance(metricService.getGlobalPerformance());
    return metricsMessage;
  }

  @Override
  public PlatformMetricsMessage getActivity() {
    final PlatformMetricsMessage metricsMessage = new PlatformMetricsMessage();
    metricsMessage.setActivity(counterService.getTenantCounters());
    return metricsMessage;
  }

  @Override
  public List<Subscription> getSubscriptions(final String entityId) {
    final Subscription subscription = new Subscription(entityId);
    return subscribeService.get(subscription);
  }

  @Override
  public void delete(final AdminInputMessage message) {
    // This method must remove everything stored in Redis related to sensors, providers or alerts.
    // The data, such as observations, orders and alarms, does not need be removed because them have
    // a ttl associated with and Redis automatically deleted them.

    // Furthermore, these data will be orphaned so them cannot be retrieved with the API REST.
    if (!CollectionUtils.isEmpty(message.getProviders())) {
      deleteProviders(message.getProviders());
    } else if (!CollectionUtils.isEmpty(message.getSensors())) {
      deleteSensors(message.getSensors());
    } else if (!CollectionUtils.isEmpty(message.getAlerts())) {
      deleteAlerts(message.getAlerts());
    }
  }

  @Override
  public void save(final AdminInputMessage message) {
    // This method gets either a sensors or alerts list and should created/update each one in Redis
    if (!CollectionUtils.isEmpty(message.getSensors())) {
      saveSensors(message);
    }

    if (!CollectionUtils.isEmpty(message.getAlerts())) {
      saveAlerts(message);
    }
  }

  private void deleteProviders(final List<CatalogProvider> providers) {
    for (final CatalogProvider provider : providers) {
      LOGGER.debug("Deleting provider [{}]", provider.getProvider());
      resourceService.removeProvider(provider.getProvider());
      subscribeService.remove(new Subscription(provider.getProvider()));
    }
  }

  private void deleteSensors(final List<CatalogSensor> sensors) {
    for (final CatalogSensor sensor : sensors) {
      LOGGER.debug("Deleting sensor [{}] belonging to provider [{}]", sensor.getSensor(), sensor.getProvider());
      resourceService.removeSensor(sensor.getSensor(), sensor.getProvider());
    }
  }

  private void deleteAlerts(final List<CatalogAlert> alerts) {
    for (final CatalogAlert alert : alerts) {
      LOGGER.debug("Deleting alert [{}] belonging to entity [{}]", alert.getId(), alert.getEntity());
      resourceService.removeAlert(alert);
    }
  }

  private void saveSensors(final AdminInputMessage message) {
    for (final CatalogSensor sensor : message.getSensors()) {
      LOGGER.debug("Saving sensor [{}] belonging to provider [{}]", sensor.getSensor(), sensor.getProvider());
      registerProviderAndSensorIfNeedBe(sensor);
    }
  }

  private void saveAlerts(final AdminInputMessage message) {
    // For each alert, if it already exist then it should be updated. Otherwise, it should be
    // created
    for (final CatalogAlert alert : message.getAlerts()) {
      LOGGER.debug("Saving alert [{}] belonging to entity [{}]", alert.getId(), alert.getEntity());
      resourceService.registerAlertIfNeedBe(alert, true);
    }
  }

  private void registerProviderAndSensorIfNeedBe(final CatalogSensor sensor) {
    // Provider should be created if it doesn't exist in Redis yet. But if sensor exists, it should
    // be updated
    resourceService.registerProviderIfNeedBe(sensor.getProvider());
    resourceService.registerSensorIfNeedBe(sensor.getProvider(), sensor.getSensor(), sensor.getState(), true);
  }

}
