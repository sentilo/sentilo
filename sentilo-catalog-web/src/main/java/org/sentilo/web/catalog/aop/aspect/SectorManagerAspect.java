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
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Entity;
import org.sentilo.web.catalog.domain.EntityResource;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.SectorResource;
import org.sentilo.web.catalog.domain.SectorResourceGranted;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.utils.enums.EntityType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

/**
 * It aspect allows to fill in the sectors attribute in each SectorResource created or updated.
 */
@org.springframework.stereotype.Component
@Aspect
public class SectorManagerAspect implements Ordered {

  private static final int ORDER = 3;

  @Autowired
  private MongoOperations mongoOps;

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.insertAll(..)) && args(sectorsResources)")
  public void insertAll(final Collection<SectorResourceGranted> sectorsResources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.updateAll(..)) && args(sectorsResources)")
  public void updateAll(final Collection<SectorResourceGranted> sectorsResources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.create(..)) && args(sectorResource)")
  public void create(final SectorResource<?> sectorResource) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.update(..)) && args(sectorResource)")
  public void update(final SectorResource<?> sectorResource) {
    // Do nothing. Pointcut definition
  }

  @Before("((update(sectorResource) || create(sectorResource)) && @annotation(auditable) )")
  public void doSimpleAdvice(final JoinPoint jp, final SectorResource<?> sectorResource, final Auditable auditable) {

    switch (auditable.actionType()) {
      case CREATE:
        addSectorsField(sectorResource);
        break;
      case UPDATE:
        keepSectorsField(sectorResource);
        break;
      default:
        break;
    }

    return;
  }

  @Before("((updateAll(sectorsResources) || insertAll(sectorsResources)) && @annotation(auditable) )")
  public void doCollectionAdvice(final JoinPoint jp, final Collection<SectorResourceGranted> sectorsResources, final Auditable auditable) {

    switch (auditable.actionType()) {
      case CREATE:
        addSectorsField(sectorsResources);
        break;
      case UPDATE:
        keepSectorsField(sectorsResources);
        break;
      default:
        break;
    }

    return;
  }

  private void keepSectorsField(final Collection<SectorResourceGranted> sectorsResources) {
    for (final SectorResourceGranted sectorResource : sectorsResources) {
      keepSectorsField(sectorResource);
    }
  }

  @SuppressWarnings("unchecked")
  private <T> void keepSectorsField(final SectorResource<T> sectorResource) {
    final Criteria criteria = Criteria.where("_id").is(sectorResource.getId());
    final SectorResource<T> resource = mongoOps.findOne(new Query(criteria), sectorResource.getClass());
    sectorResource.setSectors(resource.getSectors());
  }

  private void addSectorsField(final Collection<SectorResourceGranted> sectorsResources) {
    for (final SectorResourceGranted sectorResource : sectorsResources) {
      addSectorsField(sectorResource);
    }
  }

  private void addSectorsField(final SectorResource<?> resource) {
    // Resource either is a User or a SectorGrantedResource. If it is a User, Provider or
    // Application then nothing must be done because sectors are filled in view
    if (resource instanceof EntityResource && resource instanceof SectorResourceGranted) {
      addEntityResourceSectorsField(resource);
    }
  }

  @SuppressWarnings("unchecked")
  private <T> void addEntityResourceSectorsField(final SectorResource<T> resource) {
    final Entity entityResourceOwner = getResourceEntityOwner((EntityResource) resource);
    resource.setSectors(((SectorResource<T>) entityResourceOwner).getSectors());
  }

  private Class<?> getEntityClass(final EntityResource entityResource) {
    return EntityType.PROVIDER.equals(entityResource.getEntityType()) ? Provider.class : Application.class;
  }

  private Entity getResourceEntityOwner(final EntityResource entityResource) {
    final Class<?> collectionClass = getEntityClass(entityResource);
    final Entity entity = (Entity) mongoOps.findById(entityResource.getEntityOwner(), collectionClass);
    return entity;
  }

  @Override
  public int getOrder() {
    return ORDER;
  }

}
