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
package org.sentilo.agent.activity.monitor.event;

import java.util.Properties;

import org.sentilo.agent.common.listener.AbstractSubscriptionsInitListener;
import org.sentilo.agent.common.utils.Utils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * This process runs only once: when the agent boots, read the subscriptions registered on the file
 * subscription.properties and for each topic register a new subscription on Redis
 */
@Component
public class SubscriptionsInitListener extends AbstractSubscriptionsInitListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionsInitListener.class);

  @Autowired
  private Properties subscriptionsDef;

  @Autowired
  @Qualifier("messageListenerImpl")
  private MessageListener messageListener;

  public void subscribe() {
    // Read the subscriptions registered on the file subscription.properties and for each topic
    // register a new subscription on Redis
    LOGGER.info("Initializing activity-monitor agent subscriptions");
    if (!CollectionUtils.isEmpty(subscriptionsDef) && StringUtils.hasText(subscriptionsDef.getProperty("topics-to-index"))) {
      final String topicsToIndex = subscriptionsDef.getProperty("topics-to-index");

      LOGGER.debug("Found {} subscriptions to register", topicsToIndex.split(",").length);

      for (final String topic : topicsToIndex.split(",")) {
        registerSubscription(messageListener, Utils.buildTopic(topic));
      }

    } else {
      LOGGER.info("No found subscriptions to register");
    }
  }
}
