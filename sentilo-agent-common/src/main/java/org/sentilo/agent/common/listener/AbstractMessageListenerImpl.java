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
package org.sentilo.agent.common.listener;

import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.exception.MessageNotWritableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;

public abstract class AbstractMessageListenerImpl implements MessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(AbstractMessageListenerImpl.class);

  private final String name;
  private RedisSerializer<String> serializer = new StringRedisSerializer();
  private StringMessageConverter eventConverter = new DefaultStringMessageConverter();
  @Autowired
  private AgentMetricsCounter metricsCounters;

  public AbstractMessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;

  }

  public void onMessage(final Message message, final byte[] pattern) {
    final String info = getInfo(message);
    final String channel = getChannel(message);

    LOGGER.debug("{} -->  Received message on channel {}", name, channel);
    LOGGER.debug("{} -->  Message content {}", name, info);

    try {
      // The received message corresponds to a JSON representation of an object of type
      // EventMessage.
      final EventMessage eventMessage = (EventMessage) eventConverter.unmarshal(info, EventMessage.class);

      doWithMessage(eventMessage);
      metricsCounters.incrementInputEvents(1);
    } catch (final MessageNotWritableException mnwe) {
      LOGGER.error("Error unmarshalling message: {}. ", info, mnwe);
    }
  }

  /**
   * Every implementation of this class must override this method to implements business logic
   *
   * @param eventMessage
   */
  public abstract void doWithMessage(final EventMessage eventMessage);

  public String getName() {
    return name;
  }

  public void setMetricsCounters(final AgentMetricsCounter metricsCounters) {
    this.metricsCounters = metricsCounters;
  }

  protected String getInfo(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  protected String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }

}
