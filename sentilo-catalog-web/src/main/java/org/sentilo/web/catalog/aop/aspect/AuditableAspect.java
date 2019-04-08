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
import java.util.Date;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;

/**
 * It aspect allows to fill in the auditable fields of each resource modified into Catalog.
 */
@org.springframework.stereotype.Component
@Aspect
public class AuditableAspect implements Ordered {

  private static final int ORDER = Integer.MAX_VALUE - 2;

  @Autowired
  protected CatalogUserDetailsService userDetailsService;

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.insertAll(..)) && args(resources)")
  public void insertAll(final Collection<CatalogDocument> resources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl.updateAll(..)) && args(resources)")
  public void updateAll(final Collection<CatalogDocument> resources) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.create(..)) && args(resource)")
  public void create(final CatalogDocument resource) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.web.catalog.service.impl.*.update(..)) && args(resource)")
  public void update(final CatalogDocument resource) {
    // Do nothing. Pointcut definition
  }

  @Before("((update(resource) || create(resource)) && @annotation(auditable) )")
  public void doSimpleAdvice(final JoinPoint jp, final CatalogDocument resource, final Auditable auditable) {
    switch (auditable.actionType()) {
      case CREATE:
        touchAuditableFields(resource, true);
        break;
      case UPDATE:
        touchAuditableFields(resource, false);
        break;
      default:
        break;
    }
    return;
  }

  @Before("((updateAll(resources) || insertAll(resources)) && @annotation(auditable))")
  public void doCollectionAdvice(final JoinPoint jp, final Collection<CatalogDocument> resources, final Auditable auditable) {

    switch (auditable.actionType()) {
      case CREATE:
        touchAuditableFields(resources, true);
        break;
      case UPDATE:
        touchAuditableFields(resources, false);
        break;
      default:
        break;
    }

    return;
  }

  private void touchAuditableFields(final Collection<CatalogDocument> resources, final boolean isCreateAction) {
    for (final CatalogDocument resource : resources) {
      touchAuditableFields(resource, isCreateAction);
    }
  }

  private void touchAuditableFields(final CatalogDocument resource, final boolean isCreateAction) {
    final Date now = new Date();
    final String userName = userDetailsService.getCatalogUserDetails().getUsername();
    resource.setUpdatedAt(now);
    resource.setUpdatedBy(userName);
    if (isCreateAction) {
      resource.setCreatedAt(now);
      resource.setCreatedBy(userName);
    }
  }

  @Override
  public int getOrder() {
    return ORDER;
  }

}
