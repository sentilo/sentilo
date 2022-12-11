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

import java.util.Properties;

import org.sentilo.agent.common.utils.AgentMessagingUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Initialize agents subscriptions using Redis Publish/Subscribe.
 *
 * From 2.0.0, migrate subscriptions to {@link AgentStreamSubscriptionsInitializer} which use Redis
 * Streams
 */
public class AgentSubscriptionsInitializer implements org.springframework.context.ApplicationListener<org.springframework.context.event.ContextRefreshedEvent> {

  private static final Logger LOGGER = LoggerFactory.getLogger(AgentSubscriptionsInitializer.class);
  private static final String DEFAULT_SUBS_FIELD = "subscriptions";

  @Autowired(required = false)
  @Qualifier("messageListenerImpl")
  protected SentiloAgentMessageListener messageListener;

  @Autowired(required = false)
  protected Properties sentiloConfigProperties;

  protected String subsFieldName = DEFAULT_SUBS_FIELD;

  @Autowired(required = false)
  private RedisMessageListenerContainer listenerContainer;

  @Override
  public void onApplicationEvent(final ContextRefreshedEvent event) {
    LOGGER.info("Begin process to register agent's subscriptions ");
    subscribe();
    LOGGER.info("End of process");
  }

  public String getSubsFieldName() {
    return subsFieldName;
  }

  public void setSubsFieldName(final String subsFieldName) {
    this.subsFieldName = subsFieldName;
  }

  /**
   * This process runs only once: when the agent boots, read the subscriptions registered on the
   * properties subscriptionDef and for each one register a new subscription on Redis.
   *
   * Any subclass could override this method to customize how subscriptions are loaded.
   */
  protected void subscribe() {
    LOGGER.info("Initializing {} agent subscriptions", AgentMessagingUtils.getAgentName());
    loadSubscriptions(getMessageListener(), getSubsProperties(), getSubsFieldName());
  }

  protected SentiloAgentMessageListener getMessageListener() {
    return messageListener;
  }

  protected Properties getSubsProperties() {
    return sentiloConfigProperties;
  }

  protected void loadSubscriptions(final SentiloAgentMessageListener messageListener, final Properties subscriptionsDef, final String fieldName) {
    if (!CollectionUtils.isEmpty(subscriptionsDef) && StringUtils.hasText(subscriptionsDef.getProperty(fieldName))) {
      final String[] topics = subscriptionsDef.getProperty(fieldName).split(",");
      LOGGER.debug("Found {} subscriptions to register", topics.length);
      for (final String topic : topics) {
        registerSubscription(messageListener, topic);
        messageListener.addTopicOfInterest(topic);
      }
    } else {
      LOGGER.info("No found subscriptions to register");
    }
  }

  protected void registerSubscription(final SentiloAgentMessageListener messageListener, final String sTopic) {
    final Topic topic = AgentMessagingUtils.buildTopic(sTopic);
    listenerContainer.addMessageListener(messageListener, topic);
    LOGGER.debug("Subscription to topic {} registered successfully", topic);
  }

}
