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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.exception.DuplicateKeyException;
import org.sentilo.web.catalog.repository.ProviderRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.TenantPermissionService;
import org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl;
import org.sentilo.web.catalog.service.impl.PermissionServiceImpl;
import org.sentilo.web.catalog.service.impl.ProviderServiceImpl;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.validator.AppsAndProvidersKeyValidatorImpl;
import org.springframework.context.ApplicationContext;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class ProviderServiceImplTest {

  @InjectMocks
  private ProviderServiceImpl providerService;

  @Mock
  private PermissionServiceImpl permissionService;

  @Mock
  private TenantPermissionService tenantPermissionService;

  @Mock
  private ProviderRepository repository;

  @Mock
  private AbstractBaseCrudServiceImpl<CatalogDocument> crudService;

  @Mock
  private MongoOperations mongoOperations;

  @Mock
  private ApplicationContext context;

  @Mock
  private Provider provider;

  @Mock
  private SearchFilter filter;

  @Mock
  private Query query;

  @Mock
  private AppsAndProvidersKeyValidatorImpl entityKeyValidator;

  final String providerId = "providerId";
  final String tenantId = "tenantId";
  final String otherTenantId = "otherTenantId";

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void addGrantedTenant() {
    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);
    when(repository.findOne(providerId)).thenReturn(provider);

    providerService.addGrantedTenant(providerId, tenantId);

    verify(provider, times(1)).getTenantsAuth();
    verify(mongoOperations, times(1)).save(any(Provider.class));
  }

  @Test
  public void removeGrantedTenant() {
    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);
    when(repository.findOne(providerId)).thenReturn(provider);

    providerService.removeGrantedTenant(providerId, tenantId);

    verify(provider, times(1)).getTenantsAuth();
    verify(mongoOperations, times(1)).save(any(Provider.class));
  }

  @Test
  public void deleteFromTenantProviderExistsTest() {
    when(crudService.getMongoOps()).thenReturn(mongoOperations);
    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);
    when(mongoOperations.find(any(Query.class), eq(Provider.class))).thenReturn(Arrays.asList(provider));

    crudService.setApplicationContext(context);

    providerService.deleteFromTenant(tenantId);

    verify(permissionService, times(1)).deleteRelated(any(Provider.class));
    verify(tenantPermissionService, times(1)).deleteRelatedEntity(any(Provider.class));
  }

  @Test
  public void deleteFromTenantProviderNotExistsTest() {
    when(crudService.getMongoOps()).thenReturn(mongoOperations);
    when(mongoOperations.find(any(Query.class), eq(Provider.class))).thenReturn(new ArrayList<Provider>());

    providerService.deleteFromTenant(tenantId);

    verify(permissionService, times(0)).deleteRelated(any(Provider.class));
    verify(tenantPermissionService, times(0)).deleteRelatedEntity(any(Provider.class));
  }

  @Test
  public void isProviderFromTenant() {
    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);
    when(repository.findOne(providerId)).thenReturn(provider);

    final boolean isIt = providerService.isProviderFromTenant(providerId, tenantId);

    Assert.assertTrue(isIt);
  }

  @Test
  public void isNotProviderFromTenant() {
    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);
    when(repository.findOne(providerId)).thenReturn(provider);

    final boolean isIt = providerService.isProviderFromTenant(providerId, otherTenantId);

    Assert.assertFalse(isIt);
  }

  @Test
  public void create() {
    when(provider.getId()).thenReturn(providerId);
    when(provider.getName()).thenReturn("mockName");

    providerService.create(provider);

    verify(provider, times(0)).setName(eq(providerId));
    verify(provider).setToken(any(String.class));
    verify(entityKeyValidator, times(2)).checkIntegrityKey(eq(providerId));
    verify(permissionService).createRelated(provider);
    verify(repository).save(provider);
  }

  @Test
  public void createInMultitenantContext() {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, "true");
    final String newId = tenantId + Constants.MULTITENANT_ENTITY_ID_PREPEND_TOKEN + providerId;

    when(provider.getId()).thenReturn(providerId);
    when(provider.getTenantId()).thenReturn(tenantId);

    providerService.create(provider);

    verify(provider, times(5)).getId();
    verify(provider).setName(eq(providerId));
    verify(provider).setId(eq(newId));
    verify(provider).setToken(any(String.class));
    verify(permissionService).createRelated(provider);
    verify(repository).save(provider);
  }

  @Test
  public void createWithDefaultName() {
    when(provider.getId()).thenReturn(providerId);

    providerService.create(provider);

    verify(provider).setName(eq(providerId));
    verify(provider).setToken(any(String.class));
    verify(entityKeyValidator, times(2)).checkIntegrityKey(eq(providerId));
    verify(permissionService).createRelated(provider);
    verify(repository).save(provider);
  }

  @Test(expected = DuplicateKeyException.class)
  public void throwExceptionWhenCreate() {
    when(provider.getId()).thenReturn(providerId);
    when(repository.findOne(providerId)).thenReturn(provider);

    doThrow(new DuplicateKeyException(new Object[] {providerId})).when(entityKeyValidator).checkIntegrityKey(providerId);

    providerService.create(provider);

    verify(repository, times(0)).save(provider);
  }

}
