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
package org.sentilo.web.catalog.test.aop;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Iterator;

import org.aspectj.lang.JoinPoint;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.aop.aspect.AuditLoggerAspect;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.security.audit.AuditHandler;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.audit.AuditingActionType;

public class AuditLoggerAspectTest {

  @InjectMocks
  private AuditLoggerAspect auditLogger;

  @Mock
  private AuditHandler auditHandler;

  @Mock
  private Auditable auditable;

  @Mock
  private JoinPoint jp;

  @Mock
  private CatalogDocument resource;

  @Mock
  private Collection<CatalogDocument> resources;

  @Mock
  private Iterator<CatalogDocument> it;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void auditCreateAction() {
    when(auditable.actionType()).thenReturn(AuditingActionType.CREATE);

    auditLogger.auditAction(jp, resource, auditable);

    verify(auditHandler).logCreate(resource);
  }

  @Test
  public void auditUpdateAction() {
    when(auditable.actionType()).thenReturn(AuditingActionType.UPDATE);

    auditLogger.auditAction(jp, resource, auditable);

    verify(auditHandler).logUpdate(resource);
  }

  @Test
  public void auditDeleteAction() {
    when(auditable.actionType()).thenReturn(AuditingActionType.DELETE);

    auditLogger.auditAction(jp, resource, auditable);

    verify(auditHandler).logDelete(resource);
  }

  @Test
  public void auditMultiAction() {
    final int total = 5;
    when(resources.isEmpty()).thenReturn(false);
    when(resources.size()).thenReturn(total);
    when(resources.iterator()).thenReturn(it);
    when(it.next()).thenReturn(resource);
    when(it.hasNext()).thenReturn(true, true, true, true, true, false);

    when(auditable.actionType()).thenReturn(AuditingActionType.CREATE);

    auditLogger.auditMultiAction(jp, resources, auditable);

    verify(auditHandler, times(total)).logCreate(resource);
  }

  @Test
  public void auditDeleteAllAction() {
    final int total = 3;
    when(resources.isEmpty()).thenReturn(false);
    when(resources.size()).thenReturn(total);
    when(resources.iterator()).thenReturn(it);
    when(it.next()).thenReturn(resource);
    when(it.hasNext()).thenReturn(true, true, true, false);

    when(auditable.actionType()).thenReturn(AuditingActionType.DELETE);

    auditLogger.auditMultiAction(jp, resources, auditable);

    verify(auditHandler, times(total)).logDelete(resource);
  }

}
