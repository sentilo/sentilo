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
package org.sentilo.web.catalog.test.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.context.TenantContextImpl;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.DuplicateKeyException;
import org.sentilo.web.catalog.repository.ApplicationRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl;
import org.sentilo.web.catalog.service.impl.ApplicationServiceImpl;
import org.sentilo.web.catalog.service.impl.PermissionServiceImpl;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.AppsAndProvidersKeyValidatorImpl;
import org.sentilo.web.catalog.validator.ResourceKeyValidator;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.util.ReflectionTestUtils;

public class ApplicationServiceImplTest extends AbstractBaseCrudServiceImplTest {

  @InjectMocks
  private ApplicationServiceImpl applicationService;

  @Mock
  private PermissionServiceImpl permissionService;

  @Mock
  private ApplicationRepository repository;

  @Mock
  private AbstractBaseCrudServiceImpl<CatalogDocument> crudService;

  @Mock
  private MongoOperations mongoOperations;

  @Mock
  private Application application;

  @Mock
  private AppsAndProvidersKeyValidatorImpl entityKeyValidator;

  final String applicationId = "applicationId";
  final String tenantId = "tenantId";
  final String otherTenantId = "otherTenantId";

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void deleteFromTenant() throws Exception {
    final Application application2 = Mockito.mock(Application.class);
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", tenantId);
    when(mongoOps.find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class))).thenReturn(Arrays.asList(application, application2));

    applicationService.deleteFromTenant(tenantId);

    verify(mongoOps).find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class));
    verify(permissionService, times(2)).deleteRelated(any(Application.class));
  }

  @Test
  public void nothingToDeleteFromTenantTest() throws Exception {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", tenantId);
    when(mongoOps.find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class))).thenReturn(Collections.emptyList());

    applicationService.deleteFromTenant(tenantId);

    verify(mongoOps).find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class));
    verify(permissionService, times(0)).deleteRelated(any(Application.class));
  }

  @Test
  public void isApplicationFromTenant() {
    when(application.getId()).thenReturn(applicationId);
    when(application.getTenantId()).thenReturn(tenantId);
    when(repository.findById(applicationId)).thenReturn(Optional.of(application));

    final boolean isIt = applicationService.isApplicationFromTenant(applicationId, tenantId);

    Assert.assertTrue(isIt);
  }

  @Test
  public void isNotApplicationFromTenant() {
    when(application.getId()).thenReturn(applicationId);
    when(application.getTenantId()).thenReturn(tenantId);
    when(repository.findById(applicationId)).thenReturn(Optional.of(application));

    final boolean isIt = applicationService.isApplicationFromTenant(applicationId, otherTenantId);

    Assert.assertFalse(isIt);
  }

  @Test
  public void create() {
    when(application.getId()).thenReturn(applicationId);
    when(application.getName()).thenReturn("mockName");

    applicationService.create(application);

    verify(application, times(0)).setName(eq(applicationId));
    verify(application).setToken(any(String.class));
    verify(entityKeyValidator, times(2)).checkIntegrityKey(eq(applicationId));
    verify(permissionService).createRelated(application);
    verify(repository).save(application);
  }

  @Test
  public void createInMultitenantContext() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, "true");
    final String newId = tenantId + Constants.MULTITENANT_ENTITY_ID_PREPEND_TOKEN + applicationId;

    when(application.getId()).thenReturn(applicationId);
    when(application.getTenantId()).thenReturn(tenantId);

    applicationService.create(application);

    verify(application, times(5)).getId();
    verify(application).setName(eq(applicationId));
    verify(application).setId(eq(newId));
    verify(application).setToken(any(String.class));
    verify(permissionService).createRelated(application);
    verify(repository).save(application);
  }

  @Test
  public void createWithDefaultName() {
    when(application.getId()).thenReturn(applicationId);

    applicationService.create(application);

    verify(application).setName(eq(applicationId));
    verify(application).setToken(any(String.class));
    verify(entityKeyValidator, times(2)).checkIntegrityKey(eq(applicationId));
    verify(permissionService).createRelated(application);
    verify(repository).save(application);
  }

  @Test(expected = DuplicateKeyException.class)
  public void throwExceptionWhenCreate() {
    when(application.getId()).thenReturn(applicationId);
    when(repository.findById(applicationId)).thenReturn(Optional.of(application));

    doThrow(new DuplicateKeyException(new Object[] {applicationId})).when(entityKeyValidator).checkIntegrityKey(applicationId);

    applicationService.create(application);

    verify(repository, times(0)).save(application);
  }

  @Test
  public void getEntityId() {
    when(application.getId()).thenReturn(applicationId);

    assertEquals(applicationId, applicationService.getEntityId(application));
  }

  @Test
  public void findAllowedInSingleTenantInstance() {
    final SearchFilter filter = new SearchFilter();

    applicationService.findAllowed();

    verify(mongoOps).find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class));
  }

  @Test
  public void findAllowedInMultiTenantInstance() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    final String mockTenant = "mockTenant";
    TenantContextHolder.setContext(new TenantContextImpl(mockTenant));

    final SearchFilter filter = new SearchFilter();
    filter.addParam("tenantsAuth", mockTenant);

    applicationService.findAllowed();

    verify(mongoOps).find(argThat(new SearchFilterQueryMatcher(filter)), eq(Application.class));

    TenantContextHolder.clearContext();
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
  }

  @Test
  public void doAfterDelete() {
    applicationService.delete(application);

    verify(permissionService).deleteRelated(application);
  }

  @Test
  public void doAfterDeleteWithCollection() {
    final Application application2 = Mockito.mock(Application.class);

    applicationService.delete(Arrays.asList(application, application2));

    verify(permissionService, times(2)).deleteRelated(any(Application.class));
  }

  @Test
  public void doAfterInit() {
    final ResourceKeyValidator rkv = (ResourceKeyValidator) ReflectionTestUtils.invokeGetterMethod(applicationService, "getResourceKeyValidator");
    Assert.assertEquals(entityKeyValidator, rkv);
  }
}
