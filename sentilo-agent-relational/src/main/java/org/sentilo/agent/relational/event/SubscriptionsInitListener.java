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
package org.sentilo.agent.relational.event;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.sentilo.agent.common.listener.AbstractSubscriptionsInitListener;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.agent.relational.listener.MessageListenerImpl;
import org.sentilo.agent.relational.service.DataTrackService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Este proceso solo se ejecuta una vez: cuando la aplicación arranca. Se encarga de
 * inicializar/registrar las subscripciones definidas en el fichero de propiedades y a las cuales
 * debe estar subscrito este agente.
 */
@Component
public class SubscriptionsInitListener extends AbstractSubscriptionsInitListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscriptionsInitListener.class);

  @Autowired
  private Properties subscriptionsDef;

  @Autowired
  private DataTrackService dataTrackService;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  public void subscribe() {
    // Este proceso lee las subscripciones definidas en el fichero subscription.properties y las
    // agrupa por el Ds a utilizar para persistir los datos.
    // Para cada grupo define un MessageListener que se subscribirá cada una de las subscripciones
    // definidas.
    LOGGER.info("Initializing relational agent subscriptions");
    if (!CollectionUtils.isEmpty(subscriptionsDef)) {
      LOGGER.debug("Found {} subscriptions to register", subscriptionsDef.size());
      final Map<String, List<String>> dsGroup = new HashMap<String, List<String>>();
      validateAndProcessSubscriptions(dsGroup);

      if (!CollectionUtils.isEmpty(dsGroup)) {
        registerMessageListeners(dsGroup);
      }

    } else {
      LOGGER.info("No found subscriptions to register");
    }
  }

  private void validateAndProcessSubscriptions(final Map<String, List<String>> dsGroup) {
    final Iterator<String> subscriptionsKeys = subscriptionsDef.stringPropertyNames().iterator();

    while (subscriptionsKeys.hasNext()) {
      final String subscription = subscriptionsKeys.next();

      if (!isValidConfigSubscription(subscription)) {
        LOGGER.debug("Subscription {} is rejected because it is wrong configured", subscription);
        continue;
      }

      processSubscriptionDef(subscription, dsGroup);
    }
  }

  private boolean isValidConfigSubscription(final String subscription) {
    boolean isValid = true;
    if (!StringUtils.hasText(subscriptionsDef.getProperty(subscription))) {
      LOGGER.warn("Subscription {} has not a dataSource associated!", subscription);
      isValid = false;
    }

    if (!Utils.isValidSubscription(subscription)) {
      LOGGER.warn("Subscription {} has not a valid format", subscription);
      isValid = false;
    }

    return isValid;
  }

  private void registerMessageListeners(final Map<String, List<String>> dsGroup) {
    // Para cada Ds definido, definimos un grupo de subscripciones, y para cada grupo tendremos un
    // messageListener. Cada ML estara registrado a todas las subscripciones del grupo.
    final Iterator<String> dsKeys = dsGroup.keySet().iterator();
    while (dsKeys.hasNext()) {
      final String dsName = dsKeys.next();
      final List<String> dsSubscriptions = dsGroup.get(dsName);
      final MessageListener messageListener = createMessageListener(dsName);
      for (final String dsSubscription : dsSubscriptions) {
        registerSubscription(messageListener, Utils.buildTopic(dsSubscription));
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

  private MessageListenerImpl createMessageListener(final String dsName) {
    final MessageListenerImpl aux = new MessageListenerImpl(dsName, dataTrackService);
    aux.setMetricsCounters(metricsCounters);
    return aux;
  }
}
