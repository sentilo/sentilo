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
package org.sentilo.web.catalog.admin.service.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.sentilo.common.exception.RESTClientException;
import org.sentilo.web.catalog.admin.domain.DeletedResource;
import org.sentilo.web.catalog.admin.service.SynchronizationService;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class SynchronizationServiceImpl implements SynchronizationService {

  private static final Logger LOGGER = LoggerFactory.getLogger(SynchronizationServiceImpl.class);

  private static int PAGE_SIZE = 100;
  private static int MAX_RETRIES = 3;

  private boolean syncSensorsIsRunning = false;
  private boolean syncAlertsIsRunning = false;
  private boolean syncDeletedResourcesIsRunning = false;

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  private PlatformService platformService;

  /*
   * (non-Javadoc)0
   *
   * @see org.sentilo.web.catalog.admin.service.SynchronizationService#syncSensorsMetadata()
   */
  @Async
  public void syncSensorsMetadata() {

    if (syncSensorsIsRunning) {
      return;
    }

    try {
      syncSensorsIsRunning = true;
      syncResourcesMetadata(Sensor.class, "providerId", "sensorId", "state");
    } catch (final Exception e) {
      LOGGER.error("Sync process aborted due to an error (it will restart shortly): {} ", e.getMessage(), e);
    } finally {
      syncSensorsIsRunning = false;
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.admin.service.SynchronizationService#syncAlertsMetadata()
   */
  @Async
  public void syncAlertsMetadata() {
    if (syncAlertsIsRunning) {
      return;
    }

    try {
      syncAlertsIsRunning = true;
      syncResourcesMetadata(Alert.class, "providerId", "applicationId", "active");
    } catch (final Exception e) {
      LOGGER.error("Sync process aborted due to an error (it will restart shortly): {} ", e.getMessage(), e);
    } finally {
      syncAlertsIsRunning = false;
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.admin.service.SynchronizationService#deleteSyncResourcesMetadata()
   */
  @Async
  public void syncDeletedResourcesMetadata() {
    if (syncDeletedResourcesIsRunning) {
      return;
    }

    try {
      syncDeletedResourcesIsRunning = true;
      // DeletedResources could be a Provider, a Sensor or an Alert. Sync process is done in
      // hierarchical ordering: first providers, then sensors and finally alerts
      syncDeletedResourcesMetadata(Provider.class);
      syncDeletedResourcesMetadata(Sensor.class);
      syncDeletedResourcesMetadata(Alert.class);

    } catch (final Exception e) {
      LOGGER.error("Sync process aborted due to an error (it will restart shortly): {} ", e.getMessage(), e);
    } finally {
      syncDeletedResourcesIsRunning = false;
    }
  }

  private void syncDeletedResourcesMetadata(final Class<? extends CatalogDocument> resourceType) {
    try {
      final String deletedResourcesCollection = mongoOps.getCollectionName(DeletedResource.class);
      final Criteria criteria = Criteria.where("_class").is(resourceType.getName());
      final long total = mongoOps.count(new Query(criteria), DeletedResource.class);
      int processed = 0;
      int numPage = 0;
      int numRetries = 0;

      while (processed < total) {
        final Pageable pageable = new PageRequest(numPage, PAGE_SIZE, new Sort("_id"));
        final Query query = new Query(criteria);
        query.with(pageable);
        final List<? extends CatalogDocument> resources = mongoOps.find(query, resourceType, deletedResourcesCollection);

        final boolean successStep = syncDeletedResourcesMetadata(resources);

        if (successStep || numRetries >= MAX_RETRIES) {
          if (successStep) {
            removeDeletedResources(resources);
          }

          // If numRetries >=MAX_RETRIES then skip the current resources list and process should
          // continue. The skipped resources will be synchronized later
          numPage++;
          processed = processed + PAGE_SIZE;
          numRetries = 0;
        } else if (numRetries < MAX_RETRIES) {
          numRetries++;
        }
      }
    } catch (final Exception e) {
      LOGGER.error("Sync process aborted due to an error (it will restart shortly): {} ", e.getMessage(), e);
    }
  }

  private boolean syncDeletedResourcesMetadata(final Collection<? extends CatalogDocument> resources) {
    boolean syncOk = true;
    try {
      final PlatformAdminInputMessage message = new PlatformAdminInputMessage(resources);
      platformService.deleteResources(message);
    } catch (final RESTClientException rce) {
      final String resourcesType = CatalogUtils.getCollectionType(resources).getName();
      LOGGER.error("Error {} while synchronizing deleted resources of type {}. It will restart shortly", rce.getMessage(), resourcesType, rce);
      syncOk = false;
    }

    return syncOk;
  }

  private void syncResourcesMetadata(final Class<? extends CatalogDocument> resourceType, final String... fields) {
    final Criteria criteria = Criteria.where(Constants.SYNC_FIELD).is(null);
    final long total = mongoOps.count(new Query(criteria), resourceType);
    int processed = 0;
    int numPage = 0;
    int numRetries = 0;

    while (processed < total) {
      final Pageable pageable = new PageRequest(numPage, PAGE_SIZE, new Sort("_id"));
      final Query query = new Query(criteria);

      for (final String field : fields) {
        query.fields().include(field);
      }

      query.with(pageable);

      final List<? extends CatalogDocument> resources = mongoOps.find(query, resourceType);

      LOGGER.info("Synchronizing {} resources of type {}", resources.size(), resourceType.getName());
      final boolean successStep = CollectionUtils.isEmpty(resources) ? true : syncResourcesMetadata(resources);
      LOGGER.info("Result of sync process: {}", successStep);

      if (successStep || numRetries >= MAX_RETRIES) {
        // If numRetries >=MAX_RETRIES then skip current sensor list and process should continue.
        // The skipped sensors will be synchronized later
        numPage++;
        processed = processed + PAGE_SIZE;
        numRetries = 0;
      } else if (numRetries < MAX_RETRIES) {
        numRetries++;
      }
    }
  }

  private boolean syncResourcesMetadata(final Collection<? extends CatalogDocument> resources) {
    boolean syncOk = false;
    final long startTime = System.currentTimeMillis();
    try {
      final PlatformAdminInputMessage message = new PlatformAdminInputMessage(resources);
      platformService.saveResources(message);
      syncOk = true;
    } catch (final RESTClientException rce) {
      final String resourcesType = CatalogUtils.getCollectionType(resources).getName();
      LOGGER.error("Error {} while synchronizing resources of type {}. It will retry later", rce.getMessage(), resourcesType, rce);
      syncOk = false;
    } finally {
      updateSyncState(resources, syncOk);
      final long endTime = System.currentTimeMillis();
      LOGGER.info("Sync process for {} resources finished. Time: {} ms", resources.size(), endTime - startTime);
    }

    return syncOk;
  }

  private void updateSyncState(final Collection<? extends CatalogDocument> resources, final boolean syncOk) {
    final Criteria criteria = Criteria.where("id").in(getIds(resources));
    final Query query = new Query(criteria);

    final Update update = Update.update("updatedAt", new Date());
    update.set(Constants.SYNC_FIELD, syncOk ? new Date() : null);

    mongoOps.updateMulti(query, update, CatalogUtils.getCollectionType(resources));
  }

  private void removeDeletedResources(final List<? extends CatalogDocument> resources) {
    final Criteria criteria = Criteria.where("id").in(getIds(resources));
    final Query query = new Query(criteria);

    mongoOps.remove(query, DeletedResource.class);
  }

  private Collection<String> getIds(final Collection<? extends CatalogDocument> resources) {
    final Collection<String> ids = new ArrayList<String>();
    for (final CatalogDocument document : resources) {
      ids.add(document.getId());
    }

    return ids;
  }

}
