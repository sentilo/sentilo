/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.test.notification;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.platform.common.domain.NotificationParams;
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

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(notificationContext.getNotificationParams()).thenReturn(params);
    when(notificationContext.getEntity()).thenReturn(ENTITY);
    when(context.getBean("pushRestClient", RESTClientImpl.class)).thenReturn(restClient);
    when(params.getMaxRetries()).thenReturn(3l);
  }

  @Test
  public void pushNotification() {
    service.pushNotification("mockMessage", notificationContext);

    verify(restClient).post("", "mockMessage");
    verify(context).publishEvent(any(CounterEvent.class));
  }

  @Test
  public void pushNotificationWithError() {
    doThrow(RESTClientException.class).when(restClient).post("", "mockMessage");

    service.pushNotification("mockMessage", notificationContext);

    verify(restClient).post("", "mockMessage");
    verify(context, times(0)).publishEvent(any(CounterEvent.class));
    verify(repository).save(any(NotificationRetryEvent.class));
  }

  @Test
  public void pushNotificationWithErrorWithoutRetry() {
    doThrow(RESTClientException.class).when(restClient).post("", "mockMessage");
    ReflectionTestUtils.setField(service, "retryNotificationsEnabled", Boolean.FALSE);

    service.pushNotification("mockMessage", notificationContext);

    verify(restClient).post("", "mockMessage");
    verify(context, times(0)).publishEvent(any(CounterEvent.class));
    verify(repository, times(0)).save(any(NotificationRetryEvent.class));
  }

  @Test
  public void retryNotifications() {
    final NotificationRetryEvent retryEvent = new NotificationRetryEvent("mockMessage", notificationContext, 0);
    final List<NotificationRetryEvent> eventsToRetry = buildMockList(retryEvent, 5);
    when(repository.getEventsToRetry()).thenReturn(eventsToRetry);
    doThrow(RESTClientException.class).when(restClient).post("", "mockMessage");

    service.retryNotifications();

    verify(restClient, times(5)).post("", "mockMessage");
    verify(context, times(0)).publishEvent(any(CounterEvent.class));
    verify(repository, times(2)).save(any(NotificationRetryEvent.class));

  }

  private <T> List<T> buildMockList(final T mockDocument, final long total) {
    final List<T> resources = new ArrayList<T>();
    for (int i = 0; i < total; i++) {
      resources.add(mockDocument);
    }

    return resources;
  }
}
