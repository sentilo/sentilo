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
package org.sentilo.platform.service.test.notification;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.service.monitor.CounterEvent;
import org.sentilo.platform.service.notification.NotificationDeliveryContext;
import org.sentilo.platform.service.notification.NotificationDeliveryServiceImpl;
import org.sentilo.platform.service.notification.NotificationRetryEvent;
import org.sentilo.platform.service.notification.NotificationRetryRepository;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

public class NotificationDeliveryServiceImplTest {

  private final static String ENTITY = "mockEntity";

  @InjectMocks
  private NotificationDeliveryServiceImpl service;

  @Mock
  private NotificationRetryRepository repository;

  @Mock
  private ApplicationContext context;

  @Mock
  private NotificationDeliveryContext notificationContext;

  @Mock
  private NotificationParams params;

  @Mock
  private RESTClientImpl restClient;

  @Mock
  private RateLimiterService rateLimitingService;

  private final String mockMessage =
      "{\"message\":\"27\",\"timestamp\":\"29/03/2017T13:33:58\",\"topic\":\"/data/testApp_provider/testSensor\",\"type\":\"DATA\","
          + "\"sensor\":\"testSensor\",\"provider\":\"testApp_provider\",\"time\":1490794438933,\"publisher\":\"testApp_provider\","
          + "\"publishedAt\":1490794438933,\"sender\":\"testApp_provider\"}";

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(notificationContext.getNotificationParams()).thenReturn(params);
    when(notificationContext.getEntity()).thenReturn(ENTITY);
    when(context.getBean("pushRestClient", RESTClientImpl.class)).thenReturn(restClient);
    when(params.getMaxRetries()).thenReturn(3l);
    when(rateLimitingService.allow(anyString())).thenReturn(true);
  }

  @Test
  public void pushNotification() {
    service.pushNotification(mockMessage, notificationContext);

    verify(context).getBean("pushRestClient", RESTClientImpl.class);
    verify(restClient).post(argThat(new RequestContextMatcher("", mockMessage)));
    verify(context).publishEvent(any(CounterEvent.class));
  }

  @Test
  public void pushTwoNotifications() {
    service.pushNotification(mockMessage, notificationContext);
    service.pushNotification(mockMessage, notificationContext);

    verify(context, times(1)).getBean("pushRestClient", RESTClientImpl.class);
    verify(restClient, times(2)).post(argThat(new RequestContextMatcher("", mockMessage)));
    verify(context, times(2)).publishEvent(any(CounterEvent.class));
  }

  @Test
  public void pushTwoNotEqualsNotifications() {
    // Each call to pushNotification internally calls two times to notificationContext.getEntity()
    when(notificationContext.getEntity()).thenReturn(ENTITY, ENTITY, ENTITY + "-", ENTITY + "-");
    service.pushNotification(mockMessage, notificationContext);
    service.pushNotification(mockMessage, notificationContext);

    verify(context, times(2)).getBean("pushRestClient", RESTClientImpl.class);
    verify(restClient, times(2)).post(argThat(new RequestContextMatcher("", mockMessage)));
    verify(context, times(2)).publishEvent(any(CounterEvent.class));
  }

  @Test
  public void pushNotificationWithError() {
    doThrow(RESTClientException.class).when(restClient).post(argThat(new RequestContextMatcher("", mockMessage)));

    service.pushNotification(mockMessage, notificationContext);

    verify(restClient).post(argThat(new RequestContextMatcher("", mockMessage)));
    verify(context, times(0)).publishEvent(any(CounterEvent.class));
    verify(repository).save(any(NotificationRetryEvent.class));
  }

  @Test
  public void pushNotificationWithErrorWithoutRetry() {
    doThrow(RESTClientException.class).when(restClient).post(argThat(new RequestContextMatcher("", mockMessage)));
    ReflectionTestUtils.setField(service, "retryNotificationsEnabled", Boolean.FALSE);

    service.pushNotification(mockMessage, notificationContext);

    verify(restClient).post(argThat(new RequestContextMatcher("", mockMessage)));
    verify(context, times(0)).publishEvent(any(CounterEvent.class));
    verify(repository, times(0)).save(any(NotificationRetryEvent.class));
  }

  class RequestContextMatcher extends ArgumentMatcher<RequestContext> {

    final String path;
    final String body;

    public RequestContextMatcher(final String path, final String body) {
      this.path = path;
      this.body = body;
    }

    @Override
    public boolean matches(final Object argument) {
      final RequestContext rc = (RequestContext) argument;
      return path.equals(rc.getPath()) && body.equals(rc.getBody());
    }

  }

}
