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
package org.sentilo.platform.service.impl;

import java.util.Optional;

import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.enums.SignalType;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.signal.PublishSignalService;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.event.InputMessageEvent;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.common.service.PublishService;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.redis.connection.stream.ObjectRecord;
import org.springframework.data.redis.connection.stream.StreamRecords;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;

/**
 * Base class where centralize any action related to publish events/messages/...
 */
@Service
public class PublishServiceImpl extends AbstractPlatformServiceImpl implements PublishService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PublishServiceImpl.class);

  @Autowired
  private InternalAlarmService internalAlarmService;

  @Autowired
  private ApplicationContext context;

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Autowired
  private PublishSignalService signalService;

  @Override
  public void publish(final Observation data) {
    LOGGER.debug("Publish new data event associated with provider [{}] and sensor [{}]: [{}] ", data.getProvider(), data.getSensor(),
        data.getValue());
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.DATA, data.getProvider(), data.getSensor());
    final EventMessage eventMessage = PublishMessageUtils.buildEvent(data, topic);
    publish(eventMessage, topic);

    LOGGER.debug("Data published");
  }

  @Override
  public void publish(final AlarmInputMessage message) {
    LOGGER.debug("Publish new alarm event associated with alert [{}]: [{}]", message.getAlertId(), message.getMessage());
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.ALARM, message.getAlertId());
    final EventMessage eventMessage = PublishMessageUtils.buildEvent(message, topic);
    publish(eventMessage, topic);

    LOGGER.debug("Alarm published");
  }

  @Override
  public void publish(final OrderInputMessage message) {
    LOGGER.debug("Publish new order event associated with provider [{}] and sensor [{}]: [{}] ", message.getProviderId(), message.getSensorId(),
        message.getOrder());
    final Topic topic = MessagingUtils.buildTopic(SubscribeType.ORDER, message.getProviderId(), message.getSensorId());
    final EventMessage eventMessage = PublishMessageUtils.buildEvent(message, topic);
    publish(eventMessage, topic);

    LOGGER.debug("Order published");
  }

  @Override
  public void publishGhostSensorAlarm(final Observation data) {
    final Optional<String> alarmMessage = internalAlarmService.publishGhostSensorAlarm(data);
    if (alarmMessage.isPresent()) {
      // Append alarm to STREAM
      appendToStream(EventType.ALARM, alarmMessage.get());
    }
  }

  @Override
  public void publishInboundRateLimiterAlarm(final String entity, final QuotaContext quotaContext) {
    final Optional<String> alarmMessage = internalAlarmService.publishInboundRateLimiterAlarm(entity, quotaContext);
    if (alarmMessage.isPresent()) {
      // Append alarm to STREAM
      appendToStream(EventType.ALARM, alarmMessage.get());
    }
  }

  @Override
  public void publishOutboundRateLimiterAlarm(final String entity, final QuotaContext quotaContext) {
    final Optional<String> alarmMessage = internalAlarmService.publishOutboundRateLimiterAlarm(entity, quotaContext);
    if (alarmMessage.isPresent()) {
      // Append alarm to STREAM
      appendToStream(EventType.ALARM, alarmMessage.get());
    }
  }

  @Override
  public void publishInternalSignal(final SignalType signalType) {
    // TODO : extract to constant o system property module name
    signalService.publishInternalSignal(signalType, "API-Server");
  }

  /**
   * Input messages are notified in several different ways:
   * <ul>
   * <li>Publishing a message in a Redis's channel (traditional way before Sentilo HA)
   * <li>Writing a message in a Redis's stream (new way, with Sentilo HA, to notify agents when a
   * new input message is published in Sentilo)
   * <li>Publishing a InputMessageEvent event to notify internal listeners (e.g. this is the way to
   * notify external subscription listeners)
   * </ul>
   *
   * @param eventMessage
   * @param topic
   */
  private void publish(final EventMessage eventMessage, final Topic topic) {
    final String sMessage = MessagingUtils.marshal(eventMessage);
    // Publish message to Redis channel
    redisTemplate.convertAndSend(topic.getTopic(), sMessage);
    // Append message to Redis Stream
    appendToStream(EventType.valueOf(eventMessage.getType()), sMessage);
    // Publish internal event
    context.publishEvent(new InputMessageEvent(eventMessage, topic));
  }

  private void appendToStream(final EventType eventType, final String message) {
    final ObjectRecord<String, String> record = StreamRecords.objectBacked(message).withStreamKey(MessagingUtils.getStreamName(eventType));
    redisTemplate.opsForStream().add(record);
  }

}
