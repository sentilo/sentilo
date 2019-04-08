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

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogAlertResponseMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.platform.common.domain.EntityMetadataMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.converter.CatalogAlertConverter;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.CatalogAlertHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class CatalogAlertHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";
  private static final String PROVIDER2 = "provider2";

  @InjectMocks
  private CatalogAlertHandler handler;
  @Mock
  private CatalogService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private CatalogAlertConverter parser;
  @Mock
  private CatalogAlertInputMessage message;
  @Mock
  private CatalogAlertResponseMessage responseMessage;

  @Mock
  private AuthorizationService authorizationService;
  @Mock
  private EntityMetadataRepository entityMetadataRepository;
  @Mock
  private EntityMetadataMessage entityMetadataMessage;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    when(request.getResource()).thenReturn(resource);
    when(message.getEntityId()).thenReturn(PROVIDER1);
    when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
    when(authorizationService.hasAccessToAdmin(anyString(), anyString())).thenReturn(true);
    when(entityMetadataRepository.getEntityMetadataFromId(anyString())).thenReturn(entityMetadataMessage);
  }

  @Test
  public void onGet() throws Exception {
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(service.getAuthorizedAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.OK);

    simulateRequest(HttpMethod.GET, PROVIDER1);
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(service).getAuthorizedAlerts(message);
    verify(parser).writeResponse(any(SentiloResponse.class), any(CatalogAlertResponseMessage.class));
  }

  @Test
  public void onPost() throws Exception {
    final List<CatalogAlert> alerts = getAlerts();
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);
    when(service.insertAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.OK);

    simulateRequest(HttpMethod.POST, PROVIDER1);
    handler.manageRequest(request, response);

    verify(parser).parsePostRequest(request);
    verify(service).insertAlerts(message);
  }

  @Test
  public void onDelete() throws Exception {
    when(parser.parseDeleteRequest(request, false)).thenReturn(message);
    when(service.deleteAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.OK);

    simulateRequest(HttpMethod.DELETE, PROVIDER1);
    handler.manageRequest(request, response);

    verify(parser).parseDeleteRequest(request, false);
    verify(service).deleteAlerts(message);
  }

  @Test
  public void onPostWithBadRequest() throws Exception {
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.BAD_REQUEST);

    simulateRequest(HttpMethod.POST, PROVIDER1);
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
    }
  }

  @Test
  public void onPostWithInternalServerError() throws Exception {
    final List<CatalogAlert> alerts = getAlerts();
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);
    when(service.insertAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.INTERNAL_SERVER_ERROR);

    simulateRequest(HttpMethod.POST, PROVIDER1);
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertInternalServerError(e);
    }
  }

  @Test
  public void onPostUnauthorizedAlerts() throws Exception {
    final List<CatalogAlert> alerts = getInternalAlerts();
    when(parser.parsePostRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);

    simulateRequest(HttpMethod.POST, PROVIDER1);
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertForbiddenCall(e);
    }
  }

  @Test
  public void onPut() throws Exception {
    final List<CatalogAlert> alerts = getAlerts();
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);
    when(service.updateAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.OK);

    simulateRequest(HttpMethod.PUT, PROVIDER1);
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).updateAlerts(message);
  }

  @Test
  public void onPutWithBadRequest() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.BAD_REQUEST);

    simulateRequest(HttpMethod.PUT, PROVIDER1);
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertBadRequest(e);
    }
  }

  @Test
  public void onPutWithInternalServerError() throws Exception {
    final List<CatalogAlert> alerts = getAlerts();

    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);
    when(service.updateAlerts(message)).thenReturn(responseMessage);
    when(responseMessage.getCode()).thenReturn(CatalogResponseMessage.INTERNAL_SERVER_ERROR);

    simulateRequest(HttpMethod.PUT, PROVIDER1);
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertInternalServerError(e);
    }
  }

  @Test
  public void forbiddenRequest() throws Exception {
    final List<CatalogAlert> alerts = getAlerts();

    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getAlerts()).thenReturn(alerts);
    when(authorizationService.hasAccessToAdmin(PROVIDER2, PROVIDER1)).thenReturn(false);

    simulateRequest(HttpMethod.PUT, PROVIDER1);
    try {
      when(getSentiloRequest().getEntitySource()).thenReturn(PROVIDER2);
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertForbiddenCall(e);
    }
  }

  private List<CatalogAlert> getAlerts() throws Exception {
    final List<CatalogAlert> alerts = generateRandomList(CatalogAlert.class);

    return alerts;
  }

  private List<CatalogAlert> getInternalAlerts() throws Exception {
    final List<CatalogAlert> alerts = generateRandomList(CatalogAlert.class);
    for (final CatalogAlert alert : alerts) {
      alert.setType("INTERNAL");
    }

    return alerts;
  }

  private void simulateRequest(final HttpMethod method, final String tokenProvider) throws PlatformException {
    simulateRequest(method, tokenProvider, HandlerPath.CATALOG_ALERT.getPath() + "/" + PROVIDER1);
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.CATALOG_ALERT;
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
