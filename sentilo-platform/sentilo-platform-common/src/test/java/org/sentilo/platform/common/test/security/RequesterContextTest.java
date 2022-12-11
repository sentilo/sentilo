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
package org.sentilo.platform.common.test.security;

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.security.AnonymousIdentityContext;
import org.sentilo.platform.common.security.RequesterContext;

public class RequesterContextTest {

  private final String entityId = "mockEntityId";
  private final String token = "mockToken";
  private final String tenantId = "mockTenantId";

  @Mock
  private EntityMetadataMessage credential;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(credential.getEntity()).thenReturn(entityId);
    when(credential.getToken()).thenReturn(token);
    when(credential.getTenantId()).thenReturn(tenantId);
  }

  @Test
  public void anonymousContext() {
    final RequesterContext context = new AnonymousIdentityContext(credential);

    Assert.assertTrue(context.isAnonymous());
    Assert.assertEquals(entityId, context.getEntityId());
  }

  @Test
  public void buildContext() {
    final RequesterContext context = new RequesterContext(credential);

    Assert.assertFalse(context.isAnonymous());
    Assert.assertEquals(entityId, context.getEntityId());
    Assert.assertEquals(tenantId, context.getTenantId());
    Assert.assertEquals(token, context.getToken());
    Assert.assertEquals(credential, context.getMetadata());
  }
}
