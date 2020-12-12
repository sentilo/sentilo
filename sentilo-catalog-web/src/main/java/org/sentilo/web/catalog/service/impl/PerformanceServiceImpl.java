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

import java.util.ArrayList;
import java.util.Collection;

import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.domain.PlatformPerformance;
import org.sentilo.web.catalog.domain.Performance;
import org.sentilo.web.catalog.repository.PerformanceRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.PerformanceService;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class PerformanceServiceImpl extends AbstractBaseCrudServiceImpl<Performance> implements PerformanceService {

  @Autowired
  private PerformanceRepository repository;

  @Autowired
  private PlatformService platformService;

  @Autowired
  private Environment environment;

  public PerformanceServiceImpl() {
    super(Performance.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PerformanceService#getCurrentPerformance()
   */
  public Performance getCurrentPerformance() {
    final Pageable pageable = PageRequest.of(0, 1, Direction.DESC, "timestamp");
    final SearchFilter filter = new SearchFilter(pageable);
    // Performance view always show data about the request tenant (that could be different from the
    // user tenant).
    filter.addAndParam("tenant", TenantUtils.getRequestTenant());

    final SearchFilterResult<Performance> result = search(filter);

    return CollectionUtils.isEmpty(result.getContent()) ? null : result.getContent().get(0);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.PerformanceService#deleteOldPerformanceLogs()
   */
  @Scheduled(initialDelay = 30000, fixedRate = 600000)
  public void deleteOldPerformanceLogs() {
    // Only stores the performance from the last day
    final long tsToCompare = System.currentTimeMillis() - 24 * 60 * 60 * 1000;
    final Criteria criteria = Criteria.where("timestamp").lt(tsToCompare);
    doDelete(new Query(criteria));
  }

  /**
   * Method that does a call to the PubSub server to retrieve the current platform activity and
   * stores the result into the repository
   */
  @Scheduled(initialDelay = 10000, fixedRate = 30000)
  public void getAndStorePlatformPerformance() {
    // Only stores performance if profile "local" is not active
    if (environment.acceptsProfiles("!local")) {
      final PlatformMetricsMessage metrics = platformService.getPlatformPerformance();
      final Collection<Performance> globalPerformance = new ArrayList<Performance>();
      for (final PlatformPerformance platformPerformance : metrics.getPerformance()) {
        final Performance pf = new Performance(platformPerformance);
        // Control to evict duplicates in MongoDB (view issue #316311)
        if (!exists(pf.getId())) {
          globalPerformance.add(pf);
        }
      }

      if (!CollectionUtils.isEmpty(globalPerformance)) {
        insertAll(globalPerformance);
      }
    }
  }

  @Override
  public PerformanceRepository getRepository() {
    return repository;
  }

  @Override
  public String getEntityId(final Performance entity) {
    return entity.getId();
  }
}
