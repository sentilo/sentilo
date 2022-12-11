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

import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Performance;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Statistics;
import org.sentilo.web.catalog.domain.Statistics.Accounts;
import org.sentilo.web.catalog.domain.Statistics.Devices;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.PerformanceService;
import org.sentilo.web.catalog.service.StatsService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class StatsServiceImpl extends AbstractBaseServiceImpl implements StatsService {

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private PerformanceService performanceService;

  @Override
  public Statistics getCurrentStats() {
    final Devices devices = getDevices();
    final Accounts accounts = getAccounts();
    final Performance performance = performanceService.getCurrentPerformance();

    return new Statistics(devices, performance, accounts);
  }

  private Devices getDevices() {
    final long sensors = mongoOps.count(buildCountQuery(buildFilter()), Sensor.class);
    final long routersAndGateways = mongoOps.count(buildCountQuery(getGatewayAndRoutersFilter()), Component.class);
    final long components = mongoOps.count(buildCountQuery(buildFilter()), Component.class);
    final long others = components - routersAndGateways;

    return new Devices(sensors, routersAndGateways, others);
  }

  private Accounts getAccounts() {
    final long users = mongoOps.count(buildCountQuery(buildFilter()), User.class);
    final long applications = mongoOps.count(buildCountQuery(buildFilter()), Application.class);
    final long providers = mongoOps.count(buildCountQuery(buildFilter()), Provider.class);

    return new Accounts(users, applications, providers);
  }

  private SearchFilter getGatewayAndRoutersFilter() {
    final SearchFilter filter = buildFilter();
    filter.addParam("componentType", "gateway");
    filter.addParam("componentType", "router");

    return filter;
  }

  private SearchFilter buildFilter() {
    final SearchFilter filter = new SearchFilter();
    if (TenantContextHolder.hasContext() && StringUtils.hasText(TenantUtils.getRequestTenant())) {
      // Stats view always show data about the request tenant (that could be different from the user
      // tenant).
      filter.addAndParam("tenantId", TenantUtils.getRequestTenant());
    }

    return filter;
  }

}
