/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.AdminInputMessage.AdminType;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.AdminHandler;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.parser.AdminParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

public class AdminHandlerTest extends AbstractBaseHandlerTest {

  private static final String PROVIDER1 = "provider1";

  private AdminHandler handler;
  @Mock
  private AdminService service;
  @Mock
  private SentiloRequest request;
  @Mock
  private SentiloResource resource;
  @Mock
  private SentiloResponse response;
  @Mock
  private AdminParser parser;
  @Mock
  private AdminInputMessage message;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    handler = new AdminHandler();

    handler.setAdminService(service);
    handler.setAdminParser(parser);

    when(request.getResource()).thenReturn(resource);
  }

  @Test
  public void notAllowedStatsRequest() throws Exception {
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.stats);

    simulateRequest(HttpMethod.GET, PROVIDER1, "/admin/stats");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertForbiddenCall(e);
    }
  }

  @Test
  public void statsRequest() throws Exception {
    final Statistics stats = new Statistics();
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.stats);
    when(service.getStatistics()).thenReturn(stats);

    simulateRequest(HttpMethod.GET, "sentilo-catalog", "/admin/stats");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeStatsResponse(request, response, stats);
  }

  @Test(expected = MessageValidationException.class)
  public void wrongPutRequest() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.stats);

    simulateRequest(HttpMethod.PUT, "sentilo-catalog", "/admin/stats");
    handler.manageRequest(request, response);

  }

  @Test
  public void notAllowedPutRequest() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.delete);

    simulateRequest(HttpMethod.PUT, PROVIDER1, "/admin/delete");
    try {
      handler.manageRequest(request, response);
    } catch (final PlatformException e) {
      assertForbiddenCall(e);
    }

  }

  @Test
  public void deleteRequest() throws Exception {
    when(parser.parsePutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.delete);

    simulateRequest(HttpMethod.PUT, "sentilo-catalog", "/admin/delete");
    handler.manageRequest(request, response);

    verify(parser).parsePutRequest(request);
    verify(service).delete(message);
  }

  @Override
  protected HandlerPath getHandlerPath() {
    return HandlerPath.ADMIN;
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
