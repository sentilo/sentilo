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
package org.sentilo.platform.service.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.platform.common.domain.EntitiesMetadataMessage;
import org.sentilo.platform.common.domain.PermissionsMessage;

public class CatalogResponseMessagesConverterTest {

  private StringMessageConverter converter;

  @Before
  public void setUp() throws Exception {
    converter = new DefaultStringMessageConverter();
  }

  @Test
  public void parsePermissions() throws Exception {
    final String json =
        "{\"permissions\":[{\"source\":\"prov1\",\"target\":\"prov1\",\"type\":\"WRITE\"},{\"source\":\"prov2\",\"target\":\"prov2\",\"type\":\"WRITE\"}]}";

    final PermissionsMessage message = (PermissionsMessage) converter.unmarshal(json, PermissionsMessage.class);
    assertTrue(message.getPermissions() != null && message.getPermissions().size() == 2);
  }

  @Test
  public void parseEntitiesMetadata() throws Exception {
    final String json =
        "{\"entitiesMetadata\":[{\"entity\":\"app1\",\"token\":\"token3\"},{\"entity\":\"app2\",\"token\":\"token4\"},{\"entity\":\"app3\",\"token\":\"token5\"},{\"entity\":\"prov1\",\"token\":\"token1\"}]}";
    final EntitiesMetadataMessage message = (EntitiesMetadataMessage) converter.unmarshal(json, EntitiesMetadataMessage.class);
    assertTrue(message.getEntitiesMetadata() != null && message.getEntitiesMetadata().size() == 4);
  }

  @Test
  public void parseCatalogResponse() throws Exception {
    final String json = "{\"code\":\"200\",\"errorMessage\":\"\"}";

    final CatalogResponseMessage message = (CatalogResponseMessage) converter.unmarshal(json, CatalogResponseMessage.class);
    assertTrue(message.getCode() != null);
    assertEquals(CatalogResponseMessage.OK, message.getCode());
    assertEquals("", message.getErrorMessage());
  }

}
