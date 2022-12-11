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
import org.sentilo.web.catalog.domain.Application;
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
import org.springframework.data.domain.Sort.Direction;
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
      LOGGER.warn("Previous sync sensors job is already running!");
      return;
    }

    try {
      final long startTime = System.currentTimeMillis();
      syncSensorsIsRunning = true;
      final int resourcesSync = syncResourcesMetadata(Sensor.class, "providerId", "sensorId", "state", "ttl");
      if (resourcesSync > 0) {
        LOGGER.info("Process finished. {} sensors synchronized in {} ms", resourcesSync, System.currentTimeMillis() - startTime);
      }
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
      LOGGER.warn("Previous sync alerts job is already running!");
      return;
    }

    try {
      final long startTime = System.currentTimeMillis();
      syncAlertsIsRunning = true;
      final int resourcesSync = syncResourcesMetadata(Alert.class, "providerId", "applicationId", "active");
      if (resourcesSync > 0) {
        LOGGER.info("Process finished. {} alerts synchronized in {} ms", resourcesSync, System.currentTimeMillis() - startTime);
      }
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
      LOGGER.warn("Previous sync deleted resources job is already running!");
      return;
    }

    try {
      final long startTime = System.currentTimeMillis();
      int totalResourcesSync = 0;
      syncDeletedResourcesIsRunning = true;
      // A DeletedResource could be a Provider, an Application, a Sensor or an Alert. Sync process
      // is done
      // in hierarchical ordering: first providers and applications, next sensors and finally alerts
      totalResourcesSync += syncDeletedResourcesMetadata(Application.class);
      totalResourcesSync += syncDeletedResourcesMetadata(Provider.class);
      totalResourcesSync += syncDeletedResourcesMetadata(Sensor.class);
      totalResourcesSync += syncDeletedResourcesMetadata(Alert.class);

      if (totalResourcesSync > 0) {
        LOGGER.info("Process finished. {} deleted resources synchronized in {} ms", totalResourcesSync, System.currentTimeMillis() - startTime);
      }
    } catch (final Exception e) {
      LOGGER.error("Sync process aborted due to an error (it will restart shortly): {} ", e.getMessage(), e);
    } finally {
      syncDeletedResourcesIsRunning = false;
    }
  }

  private int syncDeletedResourcesMetadata(final Class<? extends CatalogDocument> resourceType) {
    return syncResourcesMetadata(resourceType, true, new String[0]);
  }

  private int syncResourcesMetadata(final Class<? extends CatalogDocument> resourceType, final String... fields) {
    return syncResourcesMetadata(resourceType, false, fields);
  }

  private int syncResourcesMetadata(final Class<? extends CatalogDocument> resourceType, final boolean isDeleteSync, final String... fields) {
    int processed = 0;
    try {

      final String resourcesCollection = mongoOps.getCollectionName(isDeleteSync ? DeletedResource.class : resourceType);
      final Criteria criteria =
          isDeleteSync ? Criteria.where("resourceClass").is(resourceType.getName()) : Criteria.where(Constants.SYNC_FIELD).is(null);
      final long total = mongoOps.count(new Query(criteria), isDeleteSync ? DeletedResource.class : resourceType);

      int numPage = 0;
      int numRetries = 0;

      if (total > 0) {
        LOGGER.info("Initializing process for synchronize {} resources of type {}.  ", total, resourceType.getName());
      }

      LOGGER.debug("Process will be executed in blocks of size {}", PAGE_SIZE);

      final long numberOfIterations = total / PAGE_SIZE;
      int currentIteration = 0;
      int iterationKO = 0;

      while (currentIteration <= numberOfIterations) {
        // Get resources to synchronize in the current iteration.
        // If a previous iteration has failed and has reached maximum number of retries, then
        // resources from that iteration are skipped
        numPage = iterationKO;
        final Pageable pageable = PageRequest.of(numPage, PAGE_SIZE, Direction.ASC, "_id");
        final Query query = new Query(criteria);
        for (final String field : fields) {
          query.fields().include(field);
        }
        query.with(pageable);

        final List<? extends CatalogDocument> resources = mongoOps.find(query, resourceType, resourcesCollection);

        final boolean successStep =
            CollectionUtils.isEmpty(resources) ? true : (isDeleteSync ? syncDeletedResourcesMetadata(resources) : syncResourcesMetadata(resources));

        LOGGER.debug("Step {} of the sync process has finished successfully? {}.", currentIteration, successStep);

        if (successStep || numRetries >= MAX_RETRIES) {
          // If numRetries >=MAX_RETRIES then current resources list are skipped and sync process
          // should continue.
          // The skipped resources will be synchronized later in a future sync process
          currentIteration++;
          processed = successStep ? processed + resources.size() : processed;
          iterationKO = numRetries >= MAX_RETRIES ? iterationKO++ : iterationKO;
          numRetries = 0;
        } else if (numRetries < MAX_RETRIES) {
          numRetries++;
        }

      }
    } catch (final Exception e) {
      LOGGER.error("Error {} while synchronizing deleted resources of type {}. It will restart shortly", e.getMessage(), resourceType.getName(), e);
    }

    return processed;
  }

  private boolean syncDeletedResourcesMetadata(final List<? extends CatalogDocument> resources) {
    boolean syncOk = true;
    try {
      final PlatformAdminInputMessage message = new PlatformAdminInputMessage(resources);
      platformService.deleteResources(message);
    } catch (final RESTClientException rce) {
      final String resourcesType = CatalogUtils.getCollectionType(resources).getName();
      LOGGER.error("Error {} while synchronizing deleted resources of type {}. It will restart shortly", rce.getMessage(), resourcesType, rce);
      syncOk = false;
    } finally {
      if (syncOk) {
        removeDeletedResources(resources);
      }
    }

    return syncOk;
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
      LOGGER.debug("Synchronization of {} resources finished in {} ms", resources.size(), endTime - startTime);
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
