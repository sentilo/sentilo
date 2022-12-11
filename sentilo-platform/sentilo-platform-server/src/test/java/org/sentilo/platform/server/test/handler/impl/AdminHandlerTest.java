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

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.domain.PlatformConfigMessage;
import org.sentilo.common.domain.PlatformMetricsMessage;
import org.sentilo.common.domain.PlatformPerformance;
import org.sentilo.common.enums.HttpMethod;
import org.sentilo.common.metrics.SentiloArtifactsMetricsMessage;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.AdminInputMessage.AdminType;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.server.converter.AdminConverter;
import org.sentilo.platform.server.exception.MessageValidationException;
import org.sentilo.platform.server.handler.HandlerPath;
import org.sentilo.platform.server.handler.impl.AdminHandler;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.test.util.ReflectionTestUtils;

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
  private AdminConverter parser;
  @Mock
  private AdminInputMessage message;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    handler = new AdminHandler();
    ReflectionTestUtils.setField(handler, "adminService", service);
    ReflectionTestUtils.setField(handler, "parser", parser);

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

    simulateRequest(HttpMethod.GET, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/stats");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(response, stats);
  }

  @Test
  public void configRequest() {
    final PlatformConfigMessage configMessage = new PlatformConfigMessage();
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.config);
    when(service.getPlatformConfig()).thenReturn(configMessage);

    simulateRequest(HttpMethod.GET, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/config");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(response, configMessage);
  }

  @Test
  public void metricsRequest() {
    final SentiloArtifactsMetricsMessage metricsMessage = new SentiloArtifactsMetricsMessage();
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.metrics);
    when(service.getSentiloArtifactsMetrics()).thenReturn(metricsMessage);

    simulateRequest(HttpMethod.GET, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/metrics");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(response, metricsMessage);
  }

  @Test
  public void saveMetricsRequest() {
    when(parser.parsePostPutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.metrics);

    simulateRequest(HttpMethod.POST, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/metrics");
    handler.manageRequest(request, response);

    verify(parser).parsePostPutRequest(request);
    verify(service).saveArtifactsMetrics(message);
  }

  @Test(expected = MessageValidationException.class)
  public void wrongPutRequest() throws Exception {
    when(parser.parsePostPutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.stats);

    simulateRequest(HttpMethod.PUT, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/stats");
    handler.manageRequest(request, response);

  }

  @Test
  public void notAllowedPutRequest() throws Exception {
    when(parser.parsePostPutRequest(request)).thenReturn(message);
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
    when(parser.parsePostPutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.delete);

    simulateRequest(HttpMethod.PUT, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/delete");
    handler.manageRequest(request, response);

    verify(parser).parsePostPutRequest(request);
    verify(service).delete(message);
  }

  @Test
  public void saveRequest() throws Exception {
    when(parser.parsePostPutRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.save);

    simulateRequest(HttpMethod.POST, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/save");
    handler.manageRequest(request, response);

    verify(parser).parsePostPutRequest(request);
    verify(service).save(message);
  }

  @Test
  public void activityRequest() throws Exception {
    final Collection<PlatformActivity> metrics = Collections.emptyList();
    final PlatformMetricsMessage metricsMessage = new PlatformMetricsMessage();
    metricsMessage.setActivity(metrics);
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.activity);
    when(service.getActivity()).thenReturn(metricsMessage);

    simulateRequest(HttpMethod.GET, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/activity");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(response, metricsMessage);
  }

  @Test
  public void performanceRequest() throws Exception {
    final Collection<PlatformPerformance> metrics = Collections.emptyList();
    final PlatformMetricsMessage metricsMessage = new PlatformMetricsMessage();
    metricsMessage.setPerformance(metrics);
    when(parser.parseGetRequest(request)).thenReturn(message);
    when(message.getType()).thenReturn(AdminType.performance);
    when(service.getPerformance()).thenReturn(metricsMessage);

    simulateRequest(HttpMethod.GET, SentiloConstants.DEFAULT_CATALOG_ID, "/admin/performance");
    handler.manageRequest(request, response);

    verify(parser).parseGetRequest(request);
    verify(parser).writeResponse(response, metricsMessage);
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
