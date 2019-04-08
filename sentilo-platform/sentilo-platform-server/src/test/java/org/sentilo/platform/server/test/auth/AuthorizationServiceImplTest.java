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
package org.sentilo.platform.server.test.auth;

import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.platform.common.domain.PermissionMessage;
import org.sentilo.platform.common.domain.PermissionMessage.PermissionType;
import org.sentilo.platform.common.domain.PermissionsMessage;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.impl.AuthorizationServiceImpl;

public class AuthorizationServiceImplTest extends AbstractBaseTest {

  private final static String PROVIDER1 = "provider1";
  private final static String PROVIDER2 = "provider2";
  private final static String APPCLIENT1 = "appClient1";
  private final static String APPCLIENT2 = "appClient2";
  private final static String APPCLIENT3 = "appClient3";

  @InjectMocks
  private AuthorizationServiceImpl service;

  @Mock
  private CatalogService catalogService;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    when(catalogService.getPermissions()).thenReturn(getPermissions());
    service.loadActivePermissions();
  }

  @Test
  public void hasAccessToRead() {
    Assert.assertTrue(service.hasAccessToRead(PROVIDER1, APPCLIENT1));
  }

  @Test
  public void hasNotAccessToRead() {
    Assert.assertFalse(service.hasAccessToRead(PROVIDER2, APPCLIENT1));
  }

  @Test
  public void hasAccessToWrite() {
    Assert.assertTrue(service.hasAccessToWrite(PROVIDER1, APPCLIENT2));
  }

  @Test
  public void hasNotAccessToWrite() {
    Assert.assertFalse(service.hasAccessToWrite(PROVIDER2, APPCLIENT2));
  }

  @Test
  public void hasAccessToAdmin() {
    Assert.assertTrue(service.hasAccessToAdmin(PROVIDER1, APPCLIENT3));
  }

  @Test
  public void hasNotAccessToAdmin() {
    Assert.assertFalse(service.hasAccessToAdmin(PROVIDER2, APPCLIENT3));
  }

  private PermissionsMessage getPermissions() {
    final PermissionsMessage message = new PermissionsMessage();

    final PermissionMessage permission1 = new PermissionMessage(PROVIDER1, APPCLIENT1, PermissionType.READ.name());
    final PermissionMessage permission2 = new PermissionMessage(PROVIDER1, APPCLIENT2, PermissionType.WRITE.name());
    final PermissionMessage permission3 = new PermissionMessage(PROVIDER1, APPCLIENT3, PermissionType.ADMIN.name());

    message.setPermissions(Arrays.asList(permission1, permission2, permission3));

    return message;
  }

}
