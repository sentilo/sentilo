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
package org.sentilo.agent.location.event;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Component;

/**
 * This process runs only once: when the agent boots, registers a new subscription in Redis to the
 * topic /data/* .
 */
@Component
public class SubscriptionsInitListener implements org.springframework.context.ApplicationListener<org.springframework.context.event.ContextRefreshedEvent> {

  private final Logger logger = LoggerFactory.getLogger(SubscriptionsInitListener.class);

  private final String data_channel_pattern = "/data/*";

  @Autowired
  private RedisMessageListenerContainer listenerContainer;
  @Autowired
  private MessageListener messageListener;

  @Override
  public void onApplicationEvent(final ContextRefreshedEvent event) {
    logger.info("Executing call to register subscriptions process");
    subscribe();
    logger.info("End of process");
  }

  void subscribe() {
    // Este proceso subscribe el agente a todos los eventos de tipo /data que se publiquen en la
    // plataforma.
    logger.info("Initializing location agent subscription");
    listenerContainer.addMessageListener(messageListener, new PatternTopic(data_channel_pattern));
    logger.debug("Location agent has been subscribe to topic {} succesfully", data_channel_pattern);
  }

}
