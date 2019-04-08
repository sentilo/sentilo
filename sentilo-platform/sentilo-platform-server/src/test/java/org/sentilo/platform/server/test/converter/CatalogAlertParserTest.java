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
package org.sentilo.platform.server.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.server.converter.CatalogAlertConverter;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;

public class CatalogAlertParserTest {

  final static String PROVIDER_ID = "prov1";
  private CatalogAlertConverter parser;
  @Mock
  private SentiloRequest sentiloRequest;
  @Mock
  private SentiloResource resource;

  @Before
  public void setUp() throws Exception {
    final String identityKeyProvider = PROVIDER_ID;
    MockitoAnnotations.initMocks(this);
    parser = new CatalogAlertConverter();

    when(sentiloRequest.getResource()).thenReturn(resource);
    when(sentiloRequest.getEntitySource()).thenReturn(identityKeyProvider);
    when(sentiloRequest.getResourcePart(0)).thenReturn(PROVIDER_ID);
  }

  @Test
  public void parsePostRequest() throws Exception {

    final String json =
        "{\"alerts\":[{\"id\":\"ALERT1\",\"name\":\"alert_1\",\"type\":\"INTERNAL\",\"entity\":\"ENTITY1\",\"component\":\"COMP1\",\"sensor\":\"sensor1\",\"trigger\":\"EQ\",\"expression\":\"abc\"},"
            + "{\"id\":\"ALERT2\",\"name\":\"alert_2\",\"type\":\"EXTERNAL\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);

    final CatalogAlertInputMessage message = parser.parsePostRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getEntityId());
    assertNotNull(message.getAlerts());
    assertEquals(message.getAlerts().size(), 2);
    assertEquals("INTERNAL", message.getAlerts().get(0).getType());
    assertEquals("EXTERNAL", message.getAlerts().get(1).getType());
  }

  @Test(expected = JsonConverterException.class)
  public void parseInvalidPostRequest() throws Exception {
    final String json =
        "{\"sensors\":[{\"sensorId\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);

    parser.parsePostRequest(sentiloRequest);
  }

  @Test
  public void parsePutRequestWithSensors() throws Exception {
    final String json = "{\"alerts\":[{\"id\":\"ALERT1\",\"name\":\"alert_1\",\"trigger\":\"LT\",\"expression\":\"22\"},"
        + "{\"id\":\"ALERT2\",\"name\":\"alert_23\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);

    final CatalogAlertInputMessage message = parser.parsePutRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getEntityId());
    assertNotNull(message.getAlerts());
    assertEquals(message.getAlerts().size(), 2);
  }

  @Test(expected = JsonConverterException.class)
  public void parseInvalidPutRequest() throws Exception {
    final String json =
        "{\"sensors\":[{\"sensorId\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);

    parser.parsePostRequest(sentiloRequest);
  }

  @Test
  public void parseSimulateDeleteRequest() throws Exception {
    final String json = "{\"alertsIds\":[\"AL001\",\"AL002\",\"AL003\"]}";

    when(sentiloRequest.getBody()).thenReturn(json);

    final CatalogAlertInputMessage message = parser.parseDeleteRequest(sentiloRequest, true);

    assertEquals(PROVIDER_ID, message.getEntityId());
    assertNotNull(message.getAlertsIds());
    assertEquals(message.getAlertsIds().length, 3);
  }

  @Test
  public void parseDeleteRequest() throws Exception {
    final String json = "{}";

    when(sentiloRequest.getBody()).thenReturn(json);

    final CatalogAlertInputMessage message = parser.parseDeleteRequest(sentiloRequest, false);

    assertEquals(PROVIDER_ID, message.getEntityId());
    Assert.assertNull(message.getAlertsIds());
  }

}
