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
package org.sentilo.platform.server.test.handler.impl;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.converter.CatalogConverter;
import org.sentilo.platform.server.exception.ForbiddenAccessException;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.CatalogHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class CatalogHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";
  private static final String PROVIDER2 = "provider2";
  @InjectMocks
  private CatalogHandler handler;
  @Mock
  private CatalogService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private CatalogConverter parser;
  @Mock
  private CatalogInputMessage message;
  @Mock
  private CatalogDeleteInputMessage deleteMessage;
  @Mock
  private AuthorizationService authorizationService;
  @Mock
  private EntityMetadataRepository entityMetadataRepository;
  @Mock
  private EntityMetadataMessage entityMetadataMessage;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    final CatalogResponseMessage responseMessage = new CatalogResponseMessage();

    when(request.getResource()).thenReturn(resource);
    when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToAdmin(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToAdmin(PROVIDER1, PROVIDER2)).thenReturn(false);
    when(service.insertSensors(any(CatalogInputMessage.class))).thenReturn(responseMessage);
    when(service.updateSensorsOrComponents(any(CatalogInputMessage.class))).thenReturn(responseMessage);
    when(service.getAuthorizedProviders(any(CatalogInputMessage.class))).thenReturn(responseMessage);
    when(service.deleteProvider(any(CatalogInputMessage.class))).thenReturn(responseMessage);
    when(entityMetadataRepository.getEntityMetadataFromId(anyString())).thenReturn(entityMetadataMessage);
  }

  @Test
  public void getRequest() throws Exception {
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getProviderId()).thenReturn(PROVIDER1);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/catalog");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(service).getAuthorizedProviders(message);
  }

  @Test(expected = ForbiddenAccessException.class)
  public void getForbiddenRequest() throws Exception {
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getProviderId()).thenReturn(PROVIDER2);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/catalog/" + PROVIDER2);
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(service).getAuthorizedProviders(message);
  }

  @Test
  public void postRequest() throws Exception {
    final List<CatalogSensor> sensors = getSensors();
    final String body = "lo que sea";
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(message.getSensors()).thenReturn(sensors);
    when(message.getBody()).thenReturn(body);
    when(message.getProviderId()).thenReturn(PROVIDER1);

    simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
    handler.manageRequest(request, response);

    verify(parser).parsePostRequest(request);
    verify(service).insertSensors(message);
  }

  @Test
  public void deleteRequest() throws Exception {
    when(parser.parseDeleteRequest(request, false)).thenReturn(deleteMessage);
    when(deleteMessage.getProviderId()).thenReturn(PROVIDER1);
    // when(deleteMessage.getBody()).thenReturn(body);

    simulateRequest(HttpMethod.DELETE, PROVIDER1, "/catalog/provider1");
    handler.manageRequest(request, response);

    verify(parser).parseDeleteRequest(request, false);
    verify(service).deleteProvider(deleteMessage);
  }

  @Test
  public void postRequestWithoutSensors() throws Exception {
    when(parser.parsePostRequest(request)).thenReturn(message);

    simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
    }
  }

  @Test
  public void postRequestWithInternalServerError() throws Exception {
    final List<CatalogSensor> sensors = getSensors();
    final String body = "lo que sea";
    final CatalogResponseMessage responseMessage = new CatalogResponseMessage("Error al insertar sensores");
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(message.getSensors()).thenReturn(sensors);
    when(message.getBody()).thenReturn(body);
    when(message.getProviderId()).thenReturn(PROVIDER1);
    when(service.insertSensors(message)).thenReturn(responseMessage);

    simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertInternalServerError(e);
    }
  }

  @Test
  public void putRequest() throws Exception {
    final List<CatalogSensor> sensors = getSensors();
    final String body = "lo que sea";
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getSensors()).thenReturn(sensors);
    when(message.getBody()).thenReturn(body);
    when(message.getProviderId()).thenReturn(PROVIDER1);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).updateSensorsOrComponents(message);
  }

  @Test
  public void putRequestWithoutEntities() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getProviderId()).thenReturn(PROVIDER1);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
    }
  }

  @Test
  public void putRequestWithInternalServerError() throws Exception {
    final List<CatalogSensor> sensors = getSensors();
    final String body = "lo que sea";
    final CatalogResponseMessage responseMessage = new CatalogResponseMessage("Error al actualizar sensores");
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getSensors()).thenReturn(sensors);
    when(message.getBody()).thenReturn(body);
    when(message.getProviderId()).thenReturn(PROVIDER1);
    when(service.updateSensorsOrComponents(message)).thenReturn(responseMessage);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertInternalServerError(e);
    }
  }

  @Test
  public void putRequestWithoutPermission() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getProviderId()).thenReturn(PROVIDER1);

    simulateRequest(HttpMethod.PUT, PROVIDER2, "/catalog/provider1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertForbiddenCall(e);
    }
  }

  private List<CatalogSensor> getSensors() {
    final List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
    final CatalogSensor sensor1 = buildSensor("sensor1", "prov1", "desc dels ensor1", "data", "35.5 67.8", "temperatura", "C");
    final CatalogSensor sensor2 = buildSensor("sensor2", "prov1", "desc dels ensor2", "data", "35.5 67.8", "temperatura", "C");
    sensors.add(sensor1);
    sensors.add(sensor2);

    return sensors;
  }

  private CatalogSensor buildSensor(final String sensor, final String provider, final String description, final String dataType,
      final String location, final String type, final String unit) {
    final CatalogSensor catalogSensor = new CatalogSensor();
    catalogSensor.setSensor(sensor);
    catalogSensor.setProvider(provider);
    catalogSensor.setDescription(description);
    catalogSensor.setDataType(dataType);
    catalogSensor.setLocation(location);
    catalogSensor.setType(type);
    catalogSensor.setUnit(unit);

    return catalogSensor;
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.CATALOG;
  }

  @Override
  protected SentiloResource getSentiloResource() {
    return resource;
  }

  @Override
  protected SentiloRequest getSentiloRequest() {
    return request;
  }

}
