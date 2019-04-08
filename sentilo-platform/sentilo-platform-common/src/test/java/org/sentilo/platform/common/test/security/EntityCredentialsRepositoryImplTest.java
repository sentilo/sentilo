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

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.EntitiesMetadataMessage;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.security.repository.EntityMetadataRepositoryImpl;
import org.sentilo.platform.common.service.CatalogService;

public class EntityCredentialsRepositoryImplTest {

  private final String token = "646967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f";
  private final String token2 = "326967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb96f";
  private final String token3 = "326967a9f99ae76cfb836026d0015c4b80f8c0e1efbd3d261250156efd8fb943";
  private final String mockEntity = "mockEntityId";
  private final String mockEntity2 = "mockEntityId2";
  private final String mockTenant = "mockTenantId";

  @InjectMocks
  private EntityMetadataRepositoryImpl repository;

  @Mock
  private CatalogService catalogService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    final EntityMetadataMessage entityMetadata = new EntityMetadataMessage();
    entityMetadata.setToken(token);
    entityMetadata.setEntity(mockEntity);
    entityMetadata.setTenantId(mockTenant);

    final EntityMetadataMessage entityMetadata2 = new EntityMetadataMessage();
    entityMetadata2.setToken(token2);
    entityMetadata2.setEntity(mockEntity2);

    final EntitiesMetadataMessage message = new EntitiesMetadataMessage();
    message.setEntitiesMetadata(Arrays.asList(new EntityMetadataMessage[] {entityMetadata, entityMetadata2}));

    when(catalogService.getEntitiesMetadata()).thenReturn(message);

    repository.loadActiveEntitiesMetadata();
  }

  @Test
  public void validCredential() {
    final boolean validCredential = repository.containsEntityCredential(token);
    final EntityMetadataMessage entityMetadata = repository.getEntityMetadataFromToken(token);

    Assert.assertTrue(validCredential);
    Assert.assertNotNull(entityMetadata);
    Assert.assertEquals(mockEntity, entityMetadata.getEntity());
  }

  @Test
  public void noValidCredential() {
    final boolean validCredential = repository.containsEntityCredential(token3);
    final EntityMetadataMessage entityMetadata = repository.getEntityMetadataFromToken(token3);

    Assert.assertFalse(validCredential);
    Assert.assertNull(entityMetadata);
  }

  @Test
  public void getTenant() {
    final String tenant = repository.getTenantOwner(mockEntity);
    final String nullTenant = repository.getTenantOwner(mockEntity2);

    Assert.assertNotNull(tenant);
    Assert.assertNull(nullTenant);
  }

}
