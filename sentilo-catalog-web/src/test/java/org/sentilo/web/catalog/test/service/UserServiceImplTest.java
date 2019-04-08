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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.UserRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;

public class UserServiceImplTest extends AbstractBaseCrudServiceImplTest {

  private final static String USER_NAME = "user1";

  @Mock
  private UserRepository repository;

  @InjectMocks
  private UserServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service.init();
  }

  @Test
  public void create() {
    final User user = buildMockUser();
    when(repository.existsById(USER_NAME)).thenReturn(Boolean.FALSE);
    when(repository.save(any(User.class))).thenReturn(user);

    final User aux = service.create(user);

    assertEquals(aux.getName(), user.getName());
    verify(repository).existsById(USER_NAME);
    verify(repository).save(user);

  }

  @Test
  public void getEntityId() {
    final User user = buildMockUser();

    assertEquals(USER_NAME, service.getEntityId(user));
  }

  @Test
  public void deleteFromTenant() {
    final String mockTenant = "mockTenantId";
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", mockTenant);

    service.deleteFromTenant(mockTenant);

    verifyDeleteFromFilter(filter, User.class);
  }

  @Test
  public void buildQueryForIdInCollection() {
    final User user = buildMockUser();
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("userName", Arrays.asList(user.getUserName()));

    service.delete(user);

    verifyDeleteFromFilter(filter, User.class);
  }

  private User buildMockUser() {
    final User user = new User(USER_NAME);
    user.setPassword("1234");
    user.setName("usuari");
    user.setEmail("user1@sentilo.org");
    addRole(Role.ADMIN, user);

    return user;
  }

  private void addRole(final Role role, final User user) {
    final List<Role> rols = new ArrayList<Role>();
    rols.add(role);
    user.setRoles(rols);
  }
}
