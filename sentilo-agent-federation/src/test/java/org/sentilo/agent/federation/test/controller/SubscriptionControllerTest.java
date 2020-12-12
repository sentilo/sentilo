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
package org.sentilo.agent.federation.test.controller;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.federation.controller.SubscriptionController;
import org.sentilo.agent.federation.domain.FederationConfig;
import org.sentilo.agent.federation.service.LocalPlatformService;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.hmac.HMACBuilder;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.validation.BindingResult;

public class SubscriptionControllerTest {

  private final String secretKey = "VERY-SECRET-WORD";
  private final String endpoint = "http://acme-mock.io/sentilo/";

  @InjectMocks
  private SubscriptionController controller;

  @Mock
  private BindingResult result;

  @Mock
  private HttpServletRequest request;

  @Mock
  private FederationConfig fConfig;

  @Mock
  private LocalPlatformService localPlatformService;

  @Mock
  private RESTClient localRestClient;

  @Mock
  private RequestContext rc;

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(controller, "secretKey", secretKey);
    ReflectionTestUtils.setField(controller, "endpoint", endpoint);
  }

  @Test
  public void subscription() throws Exception {
    final String currentDate = DateUtils.timestampToString(System.currentTimeMillis());
    final String federatedId = "mockFederedId";
    final EventMessage message = buildMockEvent();
    final String dataHeaderValue = currentDate;
    final String currentEndpoint = String.format("%s%s", endpoint, federatedId);
    final String hmacHeaderValue = HMACBuilder.buildHeader(converter.marshal(message), currentEndpoint, secretKey, currentDate);

    when(localPlatformService.getFederatedConfig(federatedId)).thenReturn(fConfig);
    when(fConfig.getId()).thenReturn(federatedId);
    when(request.getHeader(SentiloConstants.DATE_HEADER)).thenReturn(dataHeaderValue);
    when(request.getHeader(SentiloConstants.HMAC_HEADER)).thenReturn(hmacHeaderValue);

    final ResponseEntity<String> respEntity = controller.subscription(message, federatedId, request);

    verify(localRestClient).put(any(RequestContext.class));
    Assert.assertEquals(HttpStatus.OK, respEntity.getStatusCode());
  }

  @Test
  public void badRequest() throws Exception {
    final String currentDate = DateUtils.timestampToString(System.currentTimeMillis());
    final String federatedId = "mockFederedId";
    final EventMessage message = buildMockEvent();
    final String dataHeaderValue = currentDate;
    final String hmacHeaderValue = "123456==mock_hmac_header";

    when(localPlatformService.getFederatedConfig(federatedId)).thenReturn(fConfig);
    when(fConfig.getId()).thenReturn(federatedId);
    when(request.getHeader(SentiloConstants.DATE_HEADER)).thenReturn(dataHeaderValue);
    when(request.getHeader(SentiloConstants.HMAC_HEADER)).thenReturn(hmacHeaderValue);

    final ResponseEntity<String> respEntity = controller.subscription(message, federatedId, request);

    verify(localRestClient, times(0)).put(any(RequestContext.class));
    Assert.assertEquals(HttpStatus.BAD_REQUEST, respEntity.getStatusCode());
  }

  private EventMessage buildMockEvent() {
    final EventMessage eventMessage = new EventMessage();
    eventMessage.setProvider("POWER-MOCK-PROVIDER");
    eventMessage.setComponent("MIG-BOX");
    eventMessage.setSensor("MIG-001");
    eventMessage.setPublishedAt(System.currentTimeMillis());
    eventMessage.setTime(eventMessage.getPublishedAt());
    eventMessage.setSensorType("battery");
    eventMessage.setMessage("89");
    eventMessage.setType(EventType.DATA.name());
    eventMessage.setTimestamp(DateUtils.timestampToString(eventMessage.getPublishedAt()));

    return eventMessage;
  }

}
