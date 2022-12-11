/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.event.InputMessageEvent;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.impl.PublishServiceImpl;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.connection.stream.Record;
import org.springframework.data.redis.core.StreamOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.Topic;

@SuppressWarnings("unchecked")
public class PublishServiceImplTest {

  private static final String PROVIDER = "mockProvider";
  private static final String SENSOR = "mockSensor";
  private static final String ALERT = "mockAlert";

  @Mock
  private SentiloRedisTemplate sRedisTemplate;

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private StreamOperations<String, Object, Object> streamOperations;

  @Mock
  private InternalAlarmService internalAlarmService;

  @Mock
  private Observation observation;

  @Mock
  private AlarmInputMessage alarmInputMessage;

  @Mock
  private OrderInputMessage orderInputMessage;

  @Mock
  private ApplicationContext context;

  @InjectMocks
  private PublishServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(sRedisTemplate.getTemplate()).thenReturn(redisTemplate);
    when(redisTemplate.opsForStream()).thenReturn(streamOperations);
  }

  @Test
  public void publishObservation() {
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.DATA, PROVIDER, SENSOR);
    when(observation.getProvider()).thenReturn(PROVIDER);
    when(observation.getSensor()).thenReturn(SENSOR);

    service.publish(observation);

    verify(redisTemplate).convertAndSend(eq(topic.getTopic()), anyString());
    verify(streamOperations).add(any(Record.class));
    verify(context).publishEvent(any(InputMessageEvent.class));
  }

  @Test
  public void publishAlarm() {
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.ALARM, ALERT);
    when(alarmInputMessage.getAlertId()).thenReturn(ALERT);

    service.publish(alarmInputMessage);

    verify(redisTemplate).convertAndSend(eq(topic.getTopic()), anyString());
    verify(streamOperations).add(any(Record.class));
    verify(context).publishEvent(any(InputMessageEvent.class));
  }

  @Test
  public void publishOrder() {
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.ORDER, PROVIDER, SENSOR);
    when(orderInputMessage.getProviderId()).thenReturn(PROVIDER);
    when(orderInputMessage.getSensorId()).thenReturn(SENSOR);

    service.publish(orderInputMessage);

    verify(redisTemplate).convertAndSend(eq(topic.getTopic()), anyString());
    verify(streamOperations).add(any(Record.class));
    verify(context).publishEvent(any(InputMessageEvent.class));
  }

  @Test
  public void publishGhostSensorAlarm() {
    final String message = "This is an alarm message";
    when(internalAlarmService.publishGhostSensorAlarm(observation)).thenReturn(Optional.of(message));

    service.publishGhostSensorAlarm(observation);

    verify(internalAlarmService).publishGhostSensorAlarm(observation);
    verify(streamOperations).add(any(Record.class));
  }

  @Test
  public void publishInboundRateLimiterAlarm() {
    final String message = "This is an alarm message";
    final QuotaContext qc = Mockito.mock(QuotaContext.class);
    when(internalAlarmService.publishInboundRateLimiterAlarm(PROVIDER, qc)).thenReturn(Optional.of(message));

    service.publishInboundRateLimiterAlarm(PROVIDER, qc);

    verify(internalAlarmService).publishInboundRateLimiterAlarm(PROVIDER, qc);
    verify(streamOperations).add(any(Record.class));
  }

  @Test
  public void publishOutboundRateLimiterAlarm() {
    final String message = "This is an alarm message";
    final QuotaContext qc = Mockito.mock(QuotaContext.class);
    when(internalAlarmService.publishOutboundRateLimiterAlarm(PROVIDER, qc)).thenReturn(Optional.of(message));

    service.publishOutboundRateLimiterAlarm(PROVIDER, qc);

    verify(internalAlarmService).publishOutboundRateLimiterAlarm(PROVIDER, qc);
    verify(streamOperations).add(any(Record.class));
  }

}
