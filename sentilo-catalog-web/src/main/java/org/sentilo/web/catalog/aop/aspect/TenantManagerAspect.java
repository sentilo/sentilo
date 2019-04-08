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
package org.sentilo.web.catalog.aop.aspect;

import java.util.Collection;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.EntityResource;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.util.StringUtils;

/**
 * It aspect allows to fill in the tenantId and tenantAuth fields in each TenantResource created.
 */
@org.springframework.stereotype.Component
@Aspect
public class TenantManagerAspect implements Ordered {

  private static final int ORDER = 2;

  @Autowired
  private MongoOperations mongoOps;

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.insertAll(..)) && args(tenantResources)")
  public void insertAll(final Collection<TenantResource> tenantResources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.updateAll(..)) && args(tenantResources)")
  public void updateAll(final Collection<TenantResource> tenantResources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.create(..)) && args(tenantResource)")
  public void create(final TenantResource tenantResource) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.update(..)) && args(tenantResource)")
  public void update(final TenantResource tenantResource) {
    // Do nothing. Pointcut definition
  }

  @Before("((update(tenantResource) || create(tenantResource)) && @annotation(auditable) )")
  public void doSimpleAdvice(final JoinPoint jp, final TenantResource tenantResource, final Auditable auditable) {
    if (TenantContextHolder.isEnabled()) {
      final String currentTenant = TenantUtils.getCurrentTenant();
      switch (auditable.actionType()) {
        case CREATE:
          addTenantFields(tenantResource, currentTenant);
          break;
        case UPDATE:
          keepTenantFields(tenantResource);
          break;
        default:
          break;
      }
    }
    return;
  }

  @Before("((updateAll(tenantResources) || insertAll(tenantResources)) && @annotation(auditable) )")
  public void doCollectionAdvice(final JoinPoint jp, final Collection<TenantResource> tenantResources, final Auditable auditable) {
    if (TenantContextHolder.isEnabled()) {
      switch (auditable.actionType()) {
        case CREATE:
          addTenantFields(tenantResources);
          break;
        case UPDATE:
          keepTenantFields(tenantResources);
          break;
        default:
          break;
      }
    }

    return;
  }

  private void keepTenantFields(final Collection<TenantResource> tenantResources) {
    for (final TenantResource tenantResource : tenantResources) {
      keepTenantFields(tenantResource);
    }
  }

  private void keepTenantFields(final TenantResource tenantResource) {
    final Criteria criteria = Criteria.where("_id").is(tenantResource.getId());
    final TenantResource resource = mongoOps.findOne(new Query(criteria), tenantResource.getClass());
    TenantUtils.copyTenantFields(tenantResource, resource);
  }

  private void addTenantFields(final Collection<TenantResource> tenantResources) {
    for (final TenantResource tenantResource : tenantResources) {
      if (tenantResource instanceof EntityResource) {
        addEntityResourceTenantFields(tenantResource);
      } else if (StringUtils.hasText(tenantResource.getTenantId())) {
        TenantUtils.initTenantFields(tenantResource, tenantResource.getTenantId());
      }
    }
  }

  private void addTenantFields(final TenantResource resource, final String tenantResourceOwner) {
    if (resource instanceof EntityResource) {
      addEntityResourceTenantFields(resource);
    } else {
      TenantUtils.initTenantFields(resource, tenantResourceOwner);
    }
  }

  private void addEntityResourceTenantFields(final TenantResource resource) {
    final TenantResource entityResourceOwner = getResourceEntityOwner((EntityResource) resource);
    TenantUtils.copyTenantFieldsFromEntityParent(resource, entityResourceOwner);
  }

  private boolean entityBelongsToProvider(final EntityResource entityResource) {
    if (entityResource instanceof Alert) {
      return StringUtils.hasText(((Alert) entityResource).getProviderId()) ? true : false;
    } else {
      return true;
    }
  }

  private TenantResource getResourceEntityOwner(final EntityResource entityResource) {
    final Class<?> collectionClass = entityBelongsToProvider(entityResource) ? Provider.class : Application.class;
    final Criteria criteria = Criteria.where("_id").is(entityResource.getEntityOwner());
    final TenantResource entityOwner = (TenantResource) mongoOps.find(new Query(criteria), collectionClass).get(0);
    return entityOwner;
  }

  @Override
  public int getOrder() {
    return ORDER;
  }

}
