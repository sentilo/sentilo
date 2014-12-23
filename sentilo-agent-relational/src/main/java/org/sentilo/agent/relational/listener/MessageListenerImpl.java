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
package org.sentilo.agent.relational.listener;

import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.agent.relational.business.service.DataTrackService;
import org.sentilo.agent.relational.common.domain.Alarm;
import org.sentilo.agent.relational.common.domain.EndpointMessage;
import org.sentilo.agent.relational.common.domain.Observation;
import org.sentilo.agent.relational.common.domain.Order;
import org.sentilo.agent.relational.utils.ThreadLocalProperties;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SubscribeType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.Message;

public class MessageListenerImpl extends AbstractMessageListenerImpl {

  private final Logger logger = LoggerFactory.getLogger(MessageListenerImpl.class);

  private DataTrackService dataTrackService;

  public MessageListenerImpl(final String name, final DataTrackService dataTrackService) {
    // name is equals to the dataSource name to use to persist the messages received by this
    // listener
    super(name);
    this.dataTrackService = dataTrackService;
  }

  public void doWithMessage(final Message message, final EventMessage eventMessage) {
    ThreadLocalProperties.unset();
    ThreadLocalProperties.set(getName());

    try {
      final String channel = getChannel(message);
      final EndpointMessage endpointMessage = new EndpointMessage(eventMessage, channel);
      switch (getTopicType(channel)) {
        case DATA:
          final Observation observation = endpointMessage.getObservation();
          observation.setTargetDs(getName());
          dataTrackService.save(observation);
          break;
        case ALARM:
          final Alarm alarm = endpointMessage.getAlarm();
          alarm.setTargetDs(getName());
          dataTrackService.save(alarm);
          break;
        case ORDER:
          final Order order = endpointMessage.getOrder();
          order.setTargetDs(getName());
          dataTrackService.save(order);
          break;
      }

    } catch (final DataAccessException e) {
      logger.error("Error processing message. Error: {} ", e);
    }
  }

  private SubscribeType getTopicType(final String topic) {
    final String[] tokens = topic.split(Constants.REDIS_CHANNEL_TOKEN);
    return SubscribeType.valueOf(tokens[1].toUpperCase());
  }

}
