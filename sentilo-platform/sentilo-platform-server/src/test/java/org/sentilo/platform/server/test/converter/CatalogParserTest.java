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
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.server.converter.CatalogConverter;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;

public class CatalogParserTest {

  final static String PROVIDER_ID = "prov1";
  private CatalogConverter parser;
  @Mock
  private SentiloRequest sentiloRequest;
  @Mock
  private SentiloResource resource;

  @Before
  public void setUp() throws Exception {
    final String identityKeyProvider = PROVIDER_ID;
    MockitoAnnotations.initMocks(this);
    parser = new CatalogConverter();

    when(sentiloRequest.getResource()).thenReturn(resource);
    when(sentiloRequest.getEntitySource()).thenReturn(identityKeyProvider);
  }

  @Test
  public void parsePostRequest() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"sensors\":[{\"sensor\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"},"
            + "{\"sensor\":\"REC013\",\"description\":\"sensor 13\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\",\"component\":\"comp01\",\"componentDesc\":\"desc del comp\"},"
            + "{\"sensor\":\"REC014\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\",\"publicAccess\":true,\"additionalInfo\":{\"field1\":\"mock value\"},\"componentAdditionalInfo\":{\"field2\":\"mock component value\"}}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogInputMessage message = parser.parsePostRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNotNull(message.getSensors());
    assertEquals(message.getSensors().size(), 3);
    assertNotNull(message.getSensors().get(2).getAdditionalInfo());
    assertNotNull(message.getSensors().get(2).getComponentAdditionalInfo());
    assertNull(message.getSensors().get(2).getTechnicalDetails());
    assertNull(message.getSensors().get(2).getComponentTechnicalDetails());
  }

  @Test
  public void parsePostRequestWithTechnicalDetails() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"sensors\":[{\"sensor\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"},"
            + "{\"sensor\":\"REC013\",\"description\":\"sensor 13\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\",\"component\":\"comp01\",\"componentDesc\":\"desc del comp\"},"
            + "{\"sensor\":\"REC014\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\",\"publicAccess\":true,\"additionalInfo\":{\"field1\":\"mock value\"},\"componentAdditionalInfo\":{\"field2\":\"mock component value\"},"
            + "\"technicalDetails\":{\"producer\":\"mock producer\", \"model\":\"mock model\", \"serialNumber\":\"123456789\"}}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogInputMessage message = parser.parsePostRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNotNull(message.getSensors());
    assertEquals(message.getSensors().size(), 3);
    assertNotNull(message.getSensors().get(2).getAdditionalInfo());
    assertNotNull(message.getSensors().get(2).getComponentAdditionalInfo());
    assertNotNull(message.getSensors().get(2).getTechnicalDetails());
    assertNull(message.getSensors().get(2).getComponentTechnicalDetails());
  }

  @Test(expected = JsonConverterException.class)
  public void parseInvalidPostRequest() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"sensors\":[{\"sensorId\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    parser.parsePostRequest(sentiloRequest);
  }

  @Test
  public void parsePutRequestWithSensors() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"sensors\":[{\"sensor\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"},"
            + "{\"sensor\":\"REC013\",\"description\":\"sensor 13\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogInputMessage message = parser.parsePutRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNotNull(message.getSensors());
    assertNull(message.getComponents());
    assertEquals(message.getSensors().size(), 2);
  }

  @Test
  public void parsePutRequestWithComponents() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"components\":[{\"component\":\"COMP01\",\"componentDesc\":\"Comp desc\",\"location\":\"1234 5678, 1234 5678\",\"componentType\":\"humidity\", \"componentAdditionalInfo\":{\"field2\":\"mock component value\"}}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogInputMessage message = parser.parsePutRequest(sentiloRequest);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNull(message.getSensors());
    assertNotNull(message.getComponents());
    assertEquals(message.getComponents().size(), 1);
    assertNotNull(message.getComponents().get(0).getComponentAdditionalInfo());
  }

  @Test(expected = JsonConverterException.class)
  public void parseInvalidPutRequest() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json =
        "{\"sensors\":[{\"sensorId\":\"REC012\",\"description\":\"sensor 12\",\"type\":\"humidity\",\"dataType\":\"number\",\"unit\":\"grams\"}]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    parser.parsePutRequest(sentiloRequest);
  }

  @Test
  public void parseDeleteRequestWithSensors() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json = "{\"sensors\":[\"RE001\",\"RE002\",\"RE003\"]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogDeleteInputMessage message = (CatalogDeleteInputMessage) parser.parseDeleteRequest(sentiloRequest, true);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNotNull(message.getSensorsIds());
    assertNull(message.getComponentsIds());
    assertEquals(message.getSensorsIds().length, 3);
  }

  @Test
  public void parseDeleteRequestWithComponents() throws Exception {
    final String providerId = PROVIDER_ID;

    final String json = "{\"components\":[\"RE001\",\"RE002\",\"RE003\"]}";

    when(sentiloRequest.getBody()).thenReturn(json);
    when(sentiloRequest.getResourcePart(0)).thenReturn(providerId);

    final CatalogDeleteInputMessage message = (CatalogDeleteInputMessage) parser.parseDeleteRequest(sentiloRequest, true);

    assertEquals(PROVIDER_ID, message.getProviderId());
    assertNull(message.getSensorsIds());
    assertNotNull(message.getComponentsIds());
    assertEquals(message.getComponentsIds().length, 3);
  }

}
