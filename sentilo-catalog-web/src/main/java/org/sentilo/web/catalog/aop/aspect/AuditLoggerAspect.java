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
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.security.audit.AuditHandler;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;

@Component
@Aspect
public class AuditLoggerAspect implements Ordered {

  private static final int ORDER = Integer.MAX_VALUE - 1;

  @Autowired
  private AuditHandler auditHandler;

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service..*(..)) && args(resource) && @annotation(auditable)")
  public void auditAction(final JoinPoint jp, final CatalogDocument resource, final Auditable auditable) {
    switch (auditable.actionType()) {
      case CREATE:
        auditHandler.logCreate(resource);
        break;
      case UPDATE:
        auditHandler.logUpdate(resource);
        break;
      case DELETE:
        auditHandler.logDelete(resource);
        break;
      default:
        break;
    }
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service..*(..)) && @annotation(auditable)", returning = "resources")
  public void auditMultiAction(final JoinPoint jp, final Collection<CatalogDocument> resources, final Auditable auditable) {
    for (final CatalogDocument resource : resources) {
      auditAction(jp, resource, auditable);
    }
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service..*.delete(..)) && args(resources) && @annotation(auditable)")
  public void auditDeleteAllAction(final JoinPoint jp, final Collection<CatalogDocument> resources, final Auditable auditable) {
    for (final CatalogDocument resource : resources) {
      auditHandler.logDelete(resource);
    }
  }

  @Override
  public int getOrder() {
    return ORDER;
  }
}
