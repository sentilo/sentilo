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
package org.sentilo.web.catalog.test.validator;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.validator.UserValidator;
import org.springframework.validation.Errors;

public class UserValidatorTest {

  @InjectMocks
  private UserValidator userValidator;

  @Mock
  private Errors errors;

  @Mock
  private User user;

  @Before
  public void setUp() throws Exception {
    System.setProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY, Boolean.TRUE.toString());
    MockitoAnnotations.initMocks(this);
  }

  @After
  public void tearDown() {
    System.clearProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    TenantContextHolder.clearContext();
  }

  @Test
  public void supports() {
    Assert.assertTrue(userValidator.supports(User.class));
  }

  @Test
  public void roleAdminTenantMandatoryTest() {
    when(user.getRoles()).thenReturn(Arrays.asList(Role.ADMIN));

    when(user.getTenantId()).thenReturn(null);
    userValidator.validate(user, errors);
    verify(errors, times(1)).rejectValue(anyString(), anyString());
  }

  @Test
  public void roleUserTenantMandatoryTest() {
    when(user.getRoles()).thenReturn(Arrays.asList(Role.USER));

    when(user.getTenantId()).thenReturn(null);
    userValidator.validate(user, errors);
    verify(errors, times(1)).rejectValue(anyString(), anyString());
  }

  @Test
  public void rolePlatformTenantNotMandatoryTest() {
    when(user.getRoles()).thenReturn(Arrays.asList(Role.PLATFORM));

    when(user.getTenantId()).thenReturn(null);
    userValidator.validate(user, errors);
    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }

  @Test
  public void roleSuperAdminTenantNotMandatoryTest() {
    when(user.getRoles()).thenReturn(Arrays.asList(Role.SUPER_ADMIN));

    when(user.getTenantId()).thenReturn(null);
    userValidator.validate(user, errors);
    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }
}
