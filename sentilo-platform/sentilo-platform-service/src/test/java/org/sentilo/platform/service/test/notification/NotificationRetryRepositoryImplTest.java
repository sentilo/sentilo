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
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.notification.NotificationDeliveryContext;
import org.sentilo.platform.service.notification.NotificationRetryEvent;
import org.sentilo.platform.service.notification.NotificationRetryEventConverter;
import org.sentilo.platform.service.notification.NotificationRetryRepositoryImpl;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.util.CollectionUtils;

public class NotificationRetryRepositoryImplTest {

  @InjectMocks
  private NotificationRetryRepositoryImpl repository;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private NotificationRetryEvent event;

  @Mock
  private NotificationDeliveryContext deliveryContext;

  @Mock
  private NotificationParams params;

  @Mock
  private NotificationRetryEventConverter eventParser;

  @Mock
  private ZSetOperations<String, String> zSetOperations;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(event.getNotificationDeliveryContext()).thenReturn(deliveryContext);
    when(deliveryContext.getNotificationParams()).thenReturn(params);
    when(eventParser.marshall(any(NotificationRetryEvent.class))).thenReturn("");
    when(redisTemplate.opsForZSet()).thenReturn(zSetOperations);
    when(params.getRetryDelay()).thenReturn(10l);
  }

  @Test
  public void save() {
    repository.save(event);

    verify(zSetOperations).add(eq("push:pending:events"), any(String.class), any(Double.class));
  }

  @Test
  public void getEmptyEventsToRetry() {
    final long limit = 100;
    when(zSetOperations.rangeByScore(eq("push:pending:events"), eq(0d), any(Double.class), eq(0l), eq(limit)))
        .thenReturn(Collections.<String>emptySet());

    final List<NotificationRetryEvent> retryEvents = repository.getEventsToRetry(System.currentTimeMillis(), limit);

    verify(eventParser, times(0)).unmarshall(any(String.class));
    Assert.assertTrue(CollectionUtils.isEmpty(retryEvents));
  }

  @Test
  public void getEventsToRetry() {
    final long limit = 100;
    final String retryEvent = "{\"message\":\"mockMessage\", \"retryCount\":1}";
    when(zSetOperations.rangeByScore(eq("push:pending:events"), eq(0d), any(Double.class), eq(0l), eq(limit)))
        .thenReturn(new HashSet<String>(Arrays.asList(retryEvent)));

    final List<NotificationRetryEvent> retryEvents = repository.getEventsToRetry(System.currentTimeMillis(), limit);

    verify(eventParser, times(1)).unmarshall(any(String.class));
    Assert.assertTrue(retryEvents.size() == 1);
  }

}
