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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import org.aspectj.lang.JoinPoint;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.aop.aspect.TenantManagerAspect;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.audit.AuditingActionType;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class TenantManagerAspectTest {

  private final String mockTenant = "mockTenant";

  @InjectMocks
  private TenantManagerAspect advice;

  @Mock
  private MongoOperations mongoOps;

  @Mock
  private Auditable auditable;

  @Mock
  private JoinPoint jp;

  @Mock
  private TenantResource resource;

  @Mock
  private Sensor entityResource;

  @Mock
  private Component entityResource2;

  @Mock
  private Provider entityOwner;

  @Mock
  private Set<String> tenantsAuth;

  @Mock
  private Set<String> tenantsListVisible;

  @Mock
  private Set<String> tenantsMapVisible;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void create() {
    when(auditable.actionType()).thenReturn(AuditingActionType.CREATE);
    when(resource.getTenantsAuth()).thenReturn(tenantsAuth);
    when(resource.getTenantsListVisible()).thenReturn(tenantsListVisible);
    when(resource.getTenantId()).thenReturn(mockTenant);

    advice.doSimpleAdvice(jp, resource, auditable);

    verify(resource).setTenantId(mockTenant);
    verify(tenantsAuth).add(mockTenant);
    verify(tenantsListVisible).add(mockTenant);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void update() {
    final Set<String> tenantsListVisible = Collections.<String>emptySet();
    when(auditable.actionType()).thenReturn(AuditingActionType.UPDATE);
    when(mongoOps.findOne(any(Query.class), any(Class.class))).thenReturn(resource);
    when(resource.getTenantsAuth()).thenReturn(tenantsAuth);
    when(resource.getTenantsListVisible()).thenReturn(tenantsListVisible);
    when(resource.getTenantId()).thenReturn(mockTenant);

    advice.doSimpleAdvice(jp, resource, auditable);

    verify(resource).setTenantId(mockTenant);
    verify(resource).setTenantsAuth(tenantsAuth);
    verify(resource).setTenantsListVisible(tenantsListVisible);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void insertAll() {
    final Collection<TenantResource> resources = Arrays.asList(new TenantResource[] {entityResource, entityResource2});
    when(auditable.actionType()).thenReturn(AuditingActionType.CREATE);
    when(entityResource.getTenantsAuth()).thenReturn(tenantsAuth);
    when(entityResource2.getTenantsAuth()).thenReturn(tenantsAuth);
    when(entityResource2.getTenantsMapVisible()).thenReturn(tenantsMapVisible);
    when(entityResource.getTenantId()).thenReturn(mockTenant);
    when(mongoOps.find(any(Query.class), any(Class.class))).thenReturn(Arrays.asList(new Object[] {entityOwner}));
    when(entityOwner.getTenantId()).thenReturn(mockTenant);

    advice.doCollectionAdvice(jp, resources, auditable);

    verify(entityResource).setTenantId(mockTenant);
    verify(entityResource2).setTenantId(mockTenant);
    verify(tenantsAuth, times(resources.size())).addAll(entityOwner.getTenantsAuth());
    verify(tenantsListVisible, times(entityOwner.getTenantsListVisible().size())).addAll(entityOwner.getTenantsListVisible());
    verify(tenantsMapVisible).addAll(entityOwner.getTenantsAuth());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void updateAll() {
    final Collection<TenantResource> resources = Arrays.asList(new TenantResource[] {entityResource, entityResource});
    when(auditable.actionType()).thenReturn(AuditingActionType.UPDATE);
    when(mongoOps.findOne(any(Query.class), any(Class.class))).thenReturn(resource);
    when(resource.getTenantsAuth()).thenReturn(tenantsAuth);
    when(resource.getTenantsListVisible()).thenReturn(tenantsAuth);
    when(resource.getTenantId()).thenReturn(mockTenant);

    advice.doCollectionAdvice(jp, resources, auditable);

    verify(entityResource, times(resources.size())).setTenantId(mockTenant);
    verify(entityResource, times(resources.size())).setTenantsAuth(tenantsAuth);
    verify(entityResource, times(resources.size())).setTenantsListVisible(tenantsAuth);
  }

}
