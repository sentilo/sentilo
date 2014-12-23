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

import java.util.List;

import org.sentilo.web.catalog.domain.Activity;
import org.sentilo.web.catalog.domain.Statistics.Events;
import org.sentilo.web.catalog.repository.ActivityRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class ActivityServiceImpl extends AbstractBaseServiceImpl<Activity> implements ActivityService {

  @Autowired
  private ActivityRepository repository;

  private Activity lastActivity;

  public ActivityServiceImpl() {
    super(Activity.class);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.ActivityService#saveCurrentActivity(org.sentilo.web.catalog
   * .domain.Statistics.Events)
   */
  public void saveCurrentActivity(final Events currentEvents) {
    lastActivity = getLastActivity();
    if (lastActivity == null) {
      lastActivity = create(new Activity(currentEvents));
    } else {
      lastActivity = create(new Activity(currentEvents, lastActivity));
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.ActivityService#getLastRegisters()
   */
  public List<Activity> getLastActivityLogs() {
    final Pageable pageable = new PageRequest(0, 20, Direction.DESC, "timestamp");
    final SearchFilter filter = new SearchFilter(pageable);
    final SearchFilterResult<Activity> result = search(filter);
    // the list contains the last 20 entries from the collection Activity
    // in desc order, i.e., the first element is the most recent

    return result.getContent();
  }

  @Scheduled(initialDelay = 30000, fixedRate = 300000)
  public void deleteOldActivityLogs() {
    if (lastActivity != null) {
      // Solo mantenemos los ultimos 30 registros.
      final int idToCompare = Integer.parseInt(lastActivity.getId()) - 30;
      final Criteria criteria = Criteria.where("_id").lt(idToCompare);
      getMongoOps().remove(new Query(criteria), Activity.class);
    }
  }

  private Activity getLastActivity() {
    if (lastActivity == null) {
      final Pageable pageable = new PageRequest(0, 1, Direction.DESC, "timestamp");
      final SearchFilter filter = new SearchFilter(pageable);
      final SearchFilterResult<Activity> result = search(filter);

      lastActivity = (CollectionUtils.isEmpty(result.getContent()) ? null : result.getContent().get(0));
    }

    return lastActivity;
  }

  @Override
  public ActivityRepository getRepository() {
    return repository;
  }

  public void setRepository(final ActivityRepository repository) {
    this.repository = repository;
  }

  @Override
  public String getEntityId(final Activity entity) {
    return entity.getId();
  }
}
