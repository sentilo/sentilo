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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.repository.TenantRepository;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.service.impl.TenantServiceImpl;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.util.StringUtils;

public class TenantServiceImplTest extends AbstractBaseCrudServiceImplTest {

  private final String tenantId = "mockTenant-1";

  @Mock
  private Tenant tenant;

  @Mock
  private TenantRepository repository;

  @Mock
  private UserService userService;

  @Mock
  private ProviderService providerService;

  @Mock
  private ApplicationService applicationService;

  @InjectMocks
  private TenantServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service.init();
  }

  @Test
  public void getEntityId() {
    when(tenant.getId()).thenReturn(tenantId);

    assertEquals(tenantId, service.getEntityId(tenant));
  }

  @Test
  public void findPublicsButNotMe() {
    service.findPublicsButNotMe(tenantId);

    verify(mongoOps).find(argThat(new PublicTenantsQueryMatcher(tenantId)), eq(Tenant.class));
  }

  @Test
  public void findPublicTenants() {
    service.findPublicTenants();

    verify(mongoOps).find(argThat(new PublicTenantsQueryMatcher()), eq(Tenant.class));
  }

  @Test
  public void doAfterDelete() {
    when(tenant.getId()).thenReturn(tenantId);

    service.delete(tenant);

    verify(userService).deleteFromTenant(tenantId);
    verify(providerService).deleteFromTenant(tenantId);
    verify(applicationService).deleteFromTenant(tenantId);
  }

  @Test
  public void doAfterDeleteCollection() {
    final Tenant tenant2 = Mockito.mock(Tenant.class);
    final Tenant tenant3 = Mockito.mock(Tenant.class);

    service.delete(Arrays.asList(tenant, tenant2, tenant3));

    verify(userService, times(3)).deleteFromTenant(anyString());
    verify(providerService, times(3)).deleteFromTenant(anyString());
    verify(applicationService, times(3)).deleteFromTenant(anyString());
  }

  class PublicTenantsQueryMatcher extends ArgumentMatcher<Query> {

    private String tenantId = "";

    public PublicTenantsQueryMatcher() {
      super();
    }

    public PublicTenantsQueryMatcher(final String tenantId) {
      this();
      this.tenantId = tenantId;
    }

    @Override
    public boolean matches(final Object obj) {
      final Query query = (Query) obj;
      final String sQuery = query.getQueryObject().toString();
      final String sequenceToCheck = buildSequenceToCheck();

      return sQuery.contains(sequenceToCheck);
    }

    private String buildSequenceToCheck() {
      String sequenceToCheck = "";
      if (StringUtils.hasText(tenantId)) {
        sequenceToCheck = String.format("Document{{id=Document{{$ne=%s}}, isPublic=true}}", tenantId);
      } else {
        sequenceToCheck = String.format("Document{{isPublic=true}}");
      }

      return sequenceToCheck;
    }

  }

}
