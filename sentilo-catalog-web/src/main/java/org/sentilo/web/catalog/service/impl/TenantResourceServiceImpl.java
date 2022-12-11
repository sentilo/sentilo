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

import org.bson.Document;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.service.TenantResourceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import com.mongodb.BasicDBObject;
import com.mongodb.client.MongoCollection;

@Service
public class TenantResourceServiceImpl implements TenantResourceService {

  @Autowired
  private MongoOperations mongoOps;

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.TenantResourceService#removeTenantGrantFromProviderResources(
   * java.lang.String, java.lang.String)
   */
  @Override
  public void removeTenantGrantFromProviderResources(final String providerId, final String tenantToRemove) {
    updateResourceTenantsAuthByProvider(providerId, tenantToRemove, false);
    updateTenantVisibilityFromProviderResources(providerId, tenantToRemove, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.TenantResourceService#addTenantGrantToProviderResources(org.
   * sentilo.web.catalog.domain.TenantPermission)
   */
  @Override
  public void addTenantGrantToProviderResources(final TenantPermission permission) {
    updateResourceTenantsAuthByProvider(permission.getEntity(), permission.getTarget(), true);
    updateTenantVisibilityFromProviderResources(permission.getEntity(), permission.getTarget(), permission.getVisible());

  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.TenantResourceService#addTenantVisibilityToProviderResources(
   * java.lang.String, java.lang.String)
   */
  @Override
  public void addTenantVisibilityToProviderResources(final String providerId, final String tenantId) {
    updateTenantVisibilityFromProviderResources(providerId, tenantId, true);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantResourceService#
   * removeTenantVisibilityFromProviderResources(java.lang.String, java.lang.String)
   */
  @Override
  public void removeTenantVisibilityFromProviderResources(final String providerId, final String tenantToRemove) {
    updateTenantVisibilityFromProviderResources(providerId, tenantToRemove, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantResourceService#
   * addTenantListVisibilityToProviderResources(java.lang.String, java.lang.String)
   */
  @Override
  public void addTenantListVisibilityToProviderResources(final String providerId, final String tenantId) {
    updateTenantListVisibilityFromProviderResources(providerId, tenantId, true);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.TenantResourceService#
   * removeTenantListVisibilityFromProviderResources(java.lang.String, java.lang.String)
   */
  @Override
  public void removeTenantListVisibilityFromProviderResources(final String providerId, final String tenantToRemove) {
    updateTenantListVisibilityFromProviderResources(providerId, tenantToRemove, false);
  }

  private void updateResourceTenantsAuthByProvider(final String providerId, final String tenantId, final boolean addGrant) {

    // Update the Provider tenants authorization list, and list visible configuration, adding or
    // removing values as needed and updating dependent elements too (alert, application, component,
    // sensor)
    // If addGrant, then add grant to the provider and its dependent elements in cascade
    // Else, then remove the tenant grant from the provider and its dependent elements in cascade

    // Get all collections affected
    final List<String> collections = Arrays.asList("alert", "component", "sensor", "user");

    // Update the tenantsAuth and tenantsListVisible values
    if (addGrant) {
      mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
          new BasicDBObject("$push", new BasicDBObject("tenantsAuth", tenantId)));
      mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
          new BasicDBObject("$push", new BasicDBObject("tenantsListVisible", tenantId)));

      for (final String collection : collections) {
        mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
            new BasicDBObject("$push", new BasicDBObject("tenantsAuth", tenantId)));
        mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
            new BasicDBObject("$push", new BasicDBObject("tenantsListVisible", tenantId)));
      }

    } else {
      mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
          new BasicDBObject("$pull", new BasicDBObject("tenantsAuth", tenantId)));
      mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
          new BasicDBObject("$pull", new BasicDBObject("tenantsListVisible", tenantId)));

      for (final String collection : collections) {
        mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
            new BasicDBObject("$pull", new BasicDBObject("tenantsAuth", tenantId)));
        mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
            new BasicDBObject("$pull", new BasicDBObject("tenantsListVisible", tenantId)));
      }

    }
  }

  private void updateTenantVisibilityFromProviderResources(final String providerId, final String tenantId, final boolean visible) {
    if (StringUtils.hasText(tenantId)) {
      final MongoCollection<Document> coll = mongoOps.getCollection("component");
      final BasicDBObject query = new BasicDBObject("providerId", providerId);
      if (visible) {
        coll.updateMany(query, new BasicDBObject("$push", new BasicDBObject("tenantsMapVisible", tenantId)));
      } else {
        coll.updateMany(query, new BasicDBObject("$pull", new BasicDBObject("tenantsMapVisible", tenantId)));
      }
    }
  }

  private void updateTenantListVisibilityFromProviderResources(final String providerId, final String tenantId, final boolean listVisible) {

    // Add or remove the granted tenantId from the tenantResource "tenantsListVisible" field, that
    // allows to the admin user to decide if catalog must show the granted resource or not in lists
    // It must be allways a "tenantsAuth" subgroup list, so we can ever filter by it.
    // Own tenantId must be allways present

    if (StringUtils.hasText(tenantId)) {

      // Get all collections affected
      final List<String> collections = Arrays.asList("alert", "component", "sensor", "user");

      if (listVisible) {
        // Add the tenantId to the provider "tenantsListVisible" list
        mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
            new BasicDBObject("$push", new BasicDBObject("tenantsListVisible", tenantId)));
        for (final String collection : collections) {
          // Add the tenantId to the provider's dependent collection "tenantsListVisible" list
          mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
              new BasicDBObject("$push", new BasicDBObject("tenantsListVisible", tenantId)));
        }
      } else {
        // Remove the tenantId from the provider "tenantsListVisible" list
        mongoOps.getCollection("provider").updateOne(new BasicDBObject("_id", providerId),
            new BasicDBObject("$pull", new BasicDBObject("tenantsListVisible", tenantId)));
        for (final String collection : collections) {
          // Remove the tenantId from the provider's dependent collection "tenantsListVisible" list
          mongoOps.getCollection(collection).updateMany(new BasicDBObject("providerId", providerId),
              new BasicDBObject("$pull", new BasicDBObject("tenantsListVisible", tenantId)));
        }
      }

    }
  }

}
