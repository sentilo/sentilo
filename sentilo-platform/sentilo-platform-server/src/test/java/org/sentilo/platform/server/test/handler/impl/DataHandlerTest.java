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

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.service.DataService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.converter.DataConverter;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.DataHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class DataHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";
  @InjectMocks
  private DataHandler handler;
  @Mock
  private DataService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private DataConverter parser;
  @Mock
  private AuthorizationService authorizationService;
  @Mock
  private EntityMetadataRepository entityMetadataRepository;
  @Mock
  private EntityMetadataMessage entityMetadataMessage;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(request.getResource()).thenReturn(resource);
    when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
    when(entityMetadataRepository.getEntityMetadataFromId(anyString())).thenReturn(entityMetadataMessage);
  }

  @Test
  public void putRequest() throws Exception {
    final DataInputMessage message = new DataInputMessage("provider1", "sensor1", getObservations());
    when(parser.parsePutRequest(request)).thenReturn(message);
    simulateRequest(HttpMethod.PUT, PROVIDER1, "/data/provider1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).setObservations(message);
  }

  @Test
  public void invalidPutRequest() throws Exception {
    boolean errorThrown = false;
    final DataInputMessage message = new DataInputMessage("provider1", "sensor1");
    when(parser.parsePutRequest(request)).thenReturn(message);
    simulateRequest(HttpMethod.PUT, PROVIDER1, "/data/provider1/sensor1");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
      errorThrown = true;
    } finally {
      Assert.assertTrue(errorThrown);
    }
  }

  @Test(expected = EventRejectedException.class)
  public void unknownSensor() throws Exception {
    final DataInputMessage message = new DataInputMessage("provider1", "sensor1", getObservations());
    when(parser.parsePutRequest(request)).thenReturn(message);
    doThrow(EventRejectedException.class).when(service).setObservations(message);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/data/provider1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).setObservations(message);
  }

  @Test
  public void deleteRequest() throws Exception {
    final DataInputMessage message = new DataInputMessage("provider2", "sensor1");
    when(parser.parseDeleteRequest(request)).thenReturn(message);

    simulateRequest(HttpMethod.DELETE, PROVIDER1, "/data/provider2/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseDeleteRequest(request);
    verify(service).deleteLastObservations(message);
  }

  @Test
  public void getRequest() throws Exception {
    final List<Observation> observations = getObservations();
    final DataInputMessage message = new DataInputMessage("provider2", "sensor1");
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(service.getLastObservations(message)).thenReturn(observations);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/data/provider2/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(request, response, observations);
  }

  @Test
  public void simulatePutRequest() throws Exception {
    final DataInputMessage message = new DataInputMessage("provider1", "sensor1", getObservations());
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(request.getRequestParameter("method")).thenReturn("put");
    simulateRequest(HttpMethod.POST, PROVIDER1, "/data/provider1/sensor1");
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).setObservations(message);
  }

  @Test
  public void postRequest() throws Exception {
    try {
      simulateRequest(HttpMethod.POST, PROVIDER1, "/data/provider2/sensor1");
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertMethodNotAllowed(e);
    }
  }

  private List<Observation> getObservations() {
    final List<Observation> observations = new ArrayList<Observation>();
    final Observation obs1 = new Observation("prov1", "sensor1", "1", System.currentTimeMillis());
    final Observation obs2 = new Observation("prov1", "sensor1", "1", System.currentTimeMillis() + 6000);
    observations.add(obs1);
    observations.add(obs2);

    return observations;
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.DATA;
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
