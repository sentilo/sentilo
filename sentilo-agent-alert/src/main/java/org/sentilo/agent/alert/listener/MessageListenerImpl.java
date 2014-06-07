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
package org.sentilo.agent.alert.listener;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.parser.EventMessageConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

public class MessageListenerImpl implements MessageListener {

  private final Logger logger = LoggerFactory.getLogger(MessageListenerImpl.class);

  private final String name;
  private final RedisSerializer<String> serializer = new StringRedisSerializer();
  private final EventMessageConverter eventConverter = new EventMessageConverter();

  private RedisTemplate<String, String> redisTemplate;
  private final TriggerEvaluator triggerEvaluator;

  private List<Alarm> alarms;

  private boolean validValue;

  public MessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;
    triggerEvaluator = new TriggerEvaluator();
  }

  public MessageListenerImpl(final String name, final RedisTemplate<String, String> redisTemplate) {
    this(name);
    Assert.notNull(redisTemplate, "redisTemplate must not be NULL");
    this.redisTemplate = redisTemplate;

  }

  public void onMessage(final Message message, final byte[] pattern) {
    final String info = getInfo(message);
    final String channel = getChannel(message);

    logger.debug("{} -->  Recibido mensaje en el canal {}", name, channel);
    logger.debug("{} -->  Contenido del mensaje {}", name, info);

    // En cada mensaje recibido debemos volver a inicializar el valor de este flag
    validValue = true;

    // El mensaje recibido corresponde a una representación en JSON de un objeto de tipo
    // EventMessage.
    final EventMessage eventMessage = eventConverter.unmarshall(info);
    final String value = eventMessage.getMessage();

    // Cada mensaje recibido debe ser evaluado por cada una de las alarmas asociadas al listener
    if (!CollectionUtils.isEmpty(getAlarms())) {
      for (final Alarm alarm : getAlarms()) {
        evaluateMessage(alarm, value);
      }
    }

    // Si el valor recibido no lanza ninguna alarma, lo persistimos como ultimo valor recibido.
    if (validValue) {
      triggerEvaluator.setLastAcceptedValue(value);
    }
  }

  public void checkFrozenAlarm() {
    logger.debug("{} -->  check frozen alarms", name);
    if (!CollectionUtils.isEmpty(getAlarms())) {
      for (final Alarm alarm : getAlarms()) {
        switch (alarm.getTrigger()) {
          case FROZEN:
            checkFrozen(alarm);
            break;
          default:
            break;
        }
      }
    }
  }

  public void updateAlarms(final List<Alarm> newAlarms) {
    alarms = newAlarms;
  }

  public List<Alarm> getAlarms() {
    return alarms;
  }

  public String getName() {
    return name;
  }

  public void addAlarm(final Alarm alarm) {
    if (alarms == null) {
      alarms = new ArrayList<Alarm>();
    }

    alarms.add(alarm);
  }

  protected String getInfo(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  protected String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }

  private void evaluateMessage(final Alarm alarm, final String value) {
    final TriggerResult result = triggerEvaluator.evaluate(alarm, value);
    if (result.triggerConditionChecked()) {
      validValue = false;
      buildAndSendMessage(alarm, result);
    }
  }

  private void checkFrozen(final Alarm alarm) {
    final TriggerResult result = triggerEvaluator.checkFrozen(alarm);
    if (result.triggerConditionChecked()) {
      buildAndSendMessage(alarm, result);
    }
  }

  private void buildAndSendMessage(final Alarm alarm, final TriggerResult result) {
    final String channel = AlertUtils.buildTopicToPublishAlarm(alarm);
    final String message = AlertUtils.buildMessageToPublish(alarm, result.getAlarmMessage(), channel);
    logger.debug("Publish alarm message [{}] into channel [{}]", message, channel);
    redisTemplate.convertAndSend(channel, message);
  }

  public void setRedisTemplate(final RedisTemplate<String, String> redisTemplate) {
    this.redisTemplate = redisTemplate;
  }

}
