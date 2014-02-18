/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.UserRepository;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.impl.UserServiceImpl;

public class UserServiceImplTest {

  @Mock
  private UserRepository repository;

  private UserServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service = new UserServiceImpl();
    service.setRepository(repository);
    service.init();
  }

  @Test
  public void create() {
    final User user = new User("user1");
    user.setPassword("1234");
    user.setName("usuari");
    user.setEmail("user1@sentilo.org");
    addRole(Role.ADMIN, user);

    when(repository.findOne("user1")).thenReturn(null);
    when(repository.save(any(User.class))).thenReturn(user);

    final User aux = service.create(user);

    assertEquals(aux.getName(), user.getName());
    verify(repository).findOne("user1");
    verify(repository).save(user);

  }

  private void addRole(final Role role, final User user) {
    final List<Role> rols = new ArrayList<Role>();
    rols.add(role);
    user.setRoles(rols);
  }
}
