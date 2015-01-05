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
package org.sentilo.agent.relational.event;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.sentilo.agent.common.utils.Utils;
import org.sentilo.agent.relational.business.service.DataTrackService;
import org.sentilo.agent.relational.listener.MessageListenerImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Este proceso solo se ejecuta una vez: cuando la aplicación arranca. Se encarga de
 * inicializar/registrar las subscripciones definidas en el fichero de propiedades y a las cuales
 * debe estar subscrito este agente.
 */
@Component
public class SubscriptionsInitListener implements org.springframework.context.ApplicationListener<org.springframework.context.event.ContextRefreshedEvent> {

  private final Logger logger = LoggerFactory.getLogger(SubscriptionsInitListener.class);

  @Autowired
  private Properties subscriptionsDef;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;
  @Autowired
  private DataTrackService dataTrackService;

  @Override
  public void onApplicationEvent(final ContextRefreshedEvent event) {
    logger.info("Executing call to register subscriptions process");
    subscribe();
    logger.info("End of process");
  }

  void subscribe() {
    // Este proceso lee las subscripciones definidas en el fichero subscription.properties y las
    // agrupa por el Ds a utilizar para persistir los datos.
    // Para cada grupo define un MessageListener que se subscribirá cada una de las subscripciones
    // definidas.
    logger.info("Initializing relational agent subscriptions");
    if (!CollectionUtils.isEmpty(subscriptionsDef)) {
      logger.debug("Found {} subscriptions to register", subscriptionsDef.size());
      final Iterator<String> subscriptionsKeys = subscriptionsDef.stringPropertyNames().iterator();
      final Map<String, List<String>> dsGroup = new HashMap<String, List<String>>();

      while (subscriptionsKeys.hasNext()) {
        final String subscription = subscriptionsKeys.next();

        if (!StringUtils.hasText(subscriptionsDef.getProperty(subscription))) {
          logger.debug("Subscription {} rejected because it has not a dataSource associated", subscription);
          continue;
        }

        if (!Utils.isValidSubscription(subscription)) {
          logger.debug("Subscription {} rejected because it has not a valid format", subscription);
          continue;
        }

        processSubscriptionDef(subscription, dsGroup);
      }

      if (!CollectionUtils.isEmpty(dsGroup)) {
        registerMessageListeners(dsGroup);
      }

    } else {
      logger.info("No found subscriptions to register");
    }
  }

  private void registerMessageListeners(final Map<String, List<String>> dsGroup) {
    // Para cada Ds definido, definimos un grupo de subscripciones, y para cada grupo tendremos un
    // messageListener. Cada ML estara registrado a todas
    // las subscripciones del grupo.
    final Iterator<String> dsKeys = dsGroup.keySet().iterator();
    while (dsKeys.hasNext()) {
      final String dsName = dsKeys.next();
      final List<String> dsSubscriptions = dsGroup.get(dsName);
      final MessageListener messageListener = new MessageListenerImpl(dsName, dataTrackService);
      for (final String dsSubscription : dsSubscriptions) {
        registerSubscriptionIntoContainer(messageListener, dsSubscription);
        logger.debug("Subscription {} registered succesfully", dsSubscription);
      }
    }
  }

  private void processSubscriptionDef(final String subscription, final Map<String, List<String>> dsGroup) {
    final String[] dataSourcesNames = subscriptionsDef.getProperty(subscription).split(",");

    for (final String dataSourceName : dataSourcesNames) {
      if (!dsGroup.containsKey(dataSourceName)) {
        dsGroup.put(dataSourceName, new ArrayList<String>());
      }

      final List<String> dsSubscriptions = dsGroup.get(dataSourceName);
      dsSubscriptions.add(subscription);
    }
  }

  private void registerSubscriptionIntoContainer(final MessageListener messageListener, final String topic) {
    listenerContainer.addMessageListener(messageListener, Utils.buildTopic(topic));
  }

  public void setListenerContainer(final RedisMessageListenerContainer listenerContainer) {
    this.listenerContainer = listenerContainer;
  }

  public void setDataTrackService(final DataTrackService dataTrackService) {
    this.dataTrackService = dataTrackService;
  }
}
