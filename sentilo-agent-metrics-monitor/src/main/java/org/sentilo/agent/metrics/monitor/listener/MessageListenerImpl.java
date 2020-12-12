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
package org.sentilo.agent.metrics.monitor.listener;

import org.sentilo.agent.metrics.monitor.service.MetricsMonitorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

@Component
public class MessageListenerImpl implements MessageListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImpl.class);

  private final String name;
  private RedisSerializer<String> serializer = new StringRedisSerializer();

  @Autowired
  private MetricsMonitorService metricsMonitorService;

  public MessageListenerImpl() {
    this("Metrics-Monitor listener");
  }

  public MessageListenerImpl(final String name) {
    super();
    Assert.notNull(name, "name must not be NULL");
    this.name = name;
  }

  @Override
  public void onMessage(final Message message, final byte[] pattern) {
    final String metrics = getMetrics(message);
    final String channel = getChannel(message);

    // The received message corresponds to a JSON representation containing several metrics
    LOGGER.debug("{} -->  Received message on channel {}", name, channel);
    LOGGER.debug("{} -->  Message content {}", name, metrics);

    doWithMessage(metrics);
  }

  public void doWithMessage(final String metrics) {
    try {
      metricsMonitorService.process(metrics);
    } catch (final Exception e) {
      LOGGER.error("Error processing message: {} ", metrics, e);
    }
  }

  public String getName() {
    return name;
  }

  protected String getMetrics(final Message message) {
    return serializer.deserialize(message.getBody());
  }

  protected String getChannel(final Message message) {
    return serializer.deserialize(message.getChannel());
  }
}
