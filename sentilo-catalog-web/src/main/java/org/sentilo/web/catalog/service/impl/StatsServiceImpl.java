/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import java.util.Set;

import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Statistics;
import org.sentilo.web.catalog.domain.Statistics.Accounts;
import org.sentilo.web.catalog.domain.Statistics.Devices;
import org.sentilo.web.catalog.domain.Statistics.Events;
import org.sentilo.web.catalog.domain.Statistics.Performance;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ActivityService;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.StatsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class StatsServiceImpl implements StatsService {

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private PlatformService platformService;

  @Autowired
  private ActivityService activityService;

  private PlatformStatsMessage currentPlatformStats;

  @Override
  public Statistics getCurrentStats() {
    final Devices devices = getDevices();
    final Accounts accounts = getAccounts();
    // PlatformStatsMessage platformStats = getPlatformStats();
    final Performance performance = getPerformance(currentPlatformStats);
    final Events events = getEvents(currentPlatformStats);

    return new Statistics(devices, performance, events, accounts);
  }

  private Devices getDevices() {
    final long sensors = mongoOps.count(buildCountQuery(), Sensor.class);
    final long routersAndGateways = mongoOps.count(buildCountQuery(getGatewayAndRoutersFilter()), Component.class);
    final long components = mongoOps.count(buildCountQuery(), Component.class);
    final long others = components - routersAndGateways;

    return new Devices(sensors, routersAndGateways, others);
  }

  private Accounts getAccounts() {
    final long users = mongoOps.count(buildCountQuery(), User.class);
    final long applications = mongoOps.count(buildCountQuery(), Application.class);
    final long providers = mongoOps.count(buildCountQuery(), Provider.class);

    return new Accounts(users, applications, providers);
  }

  private Performance getPerformance(final PlatformStatsMessage message) {
    final float instantAvg = (message != null ? message.getPerformance().getInstantAvg() : 0);
    final float dailyAvg = (message != null ? message.getPerformance().getDailyAvg() : 0);
    final float maxAvg = (message != null ? message.getPerformance().getMaxAvg() : 0);

    return new Performance(instantAvg, dailyAvg, maxAvg);
  }

  private Events getEvents(final PlatformStatsMessage message) {
    final long total = (message != null ? message.getEvents().getTotal() : 0);
    final long observations = (message != null ? message.getEvents().getObservations() : 0);
    final long alarms = (message != null ? message.getEvents().getAlarms() : 0);
    final long orders = (message != null ? message.getEvents().getOrders() : 0);

    return new Events(total, alarms, observations, orders);
  }

  @Scheduled(initialDelay = 10000, fixedRate = 300000)
  public void getAndSavePlatformActivityAndStats() {
    currentPlatformStats = platformService.getCurrentPlatformStats();
    final Events events = getEvents(currentPlatformStats);
    activityService.saveCurrentActivity(events);
  }

  protected Query buildCountQuery() {
    return this.buildCountQuery(new SearchFilter());
  }

  protected Query buildCountQuery(final SearchFilter filter) {
    Criteria queryCriteria = new Criteria();

    if (!filter.paramsIsEmpty()) {
      // params contiene la lista de filtros a aplicar en modo disjuncion, es decir, con el operador
      // OR
      // Además, la comparativa del valor siempre es mediante "contiene la palabra buscada", es
      // decir, se debe comportar como un LIKE %value% en SQL
      final Set<String> params = filter.getParams().keySet();
      final Criteria[] aCriteria = new Criteria[params.size()];
      int i = 0;
      for (final String param : params) {
        final String regexp = ".*" + filter.getParams().get(param) + ".*";
        aCriteria[i] = Criteria.where(param).regex(regexp);
        i++;
      }

      queryCriteria = queryCriteria.orOperator(aCriteria);
    }

    final Query query = new Query(queryCriteria);

    return query;
  }

  private SearchFilter getGatewayAndRoutersFilter() {
    final SearchFilter filter = new SearchFilter();
    filter.addParam("componentType", "gateway");
    filter.addParam("componentType", "router");

    return filter;
  }

}
