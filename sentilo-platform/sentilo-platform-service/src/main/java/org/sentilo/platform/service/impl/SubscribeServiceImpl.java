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
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SensorState;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.common.service.SubscribeService;
import org.sentilo.platform.service.listener.MessageListenerFactory;
import org.sentilo.platform.service.listener.MessageListenerImpl;
import org.sentilo.platform.service.listener.MockMessageListenerImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.PubSubConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class SubscribeServiceImpl extends AbstractPlatformServiceImpl implements SubscribeService {

  private static final Logger LOGGER = LoggerFactory.getLogger(SubscribeServiceImpl.class);

  @Autowired
  private RedisMessageListenerContainer listenerContainer;
  @Autowired
  private MessageListenerFactory listenerFactory;
  @Autowired
  private ResourceService resourceService;

  private StringMessageConverter converter = new DefaultStringMessageConverter();

  private final Map<String, MessageListenerImpl> listeners = new HashMap<String, MessageListenerImpl>();

  private boolean storedSubscriptionsActivated = false;
  private static final String DUMMY_TOPIC = "/trash/dummy";

  @Scheduled(initialDelay = 10000, fixedDelay = 300000)
  public void loadSubscriptions() {
    // When platform starts, all persisted subscriptions in Redis are loaded and activated
    final boolean listenerContainerRunning = listenerContainer != null && listenerContainer.isRunning();
    LOGGER.info("Listener container isRunning? {}", listenerContainerRunning);

    if (!storedSubscriptionsActivated && listenerContainerRunning) {
      LOGGER.info("Initializing subscriptions stored in Redis");
      try {
        final Set<String> storedSubscriptions = sRedisTemplate.keys(PubSubConstants.REDIS_SUBS_PATTERN_KEY);
        activateStoredSubscriptions(storedSubscriptions);
        storedSubscriptionsActivated = true;
      } catch (final Exception e) {
        storedSubscriptionsActivated = false;
      }
      LOGGER.info("Process finished");
    }
  }

  @Override
  public void subscribe(final Subscription subscription) {
    // The first step is to validate that the resource to which the subscription refers exists in
    // Sentilo. Otherwise an error is thrown
    checkTargetResourceState(subscription);

    // Al subscribirse, no sólo se debe habilitar el listener correspondiente, sino que tb se debe
    // persistir en Redis la subscripcion para la entidad de turno. De esta manera se podrán
    // iniciar los listeners asociados a las subscripciones ya existentes cuando se arranque
    // este modulo.

    // Estos registros en Redis serán del tipo Hash y habrá uno para cada entidad.
    // Es decir, para cada entidad que este subscrita a algun canal tendremos en Redis
    // una Hash con key igual a subs:<entityId> y N entradas <field, value> donde:
    // - field: cada campo de la hash corresponderá a una subscripcion , por lo que el nombre del
    // campo identificará el canal al cual se está subscrito <event_type>/element_id, donde
    // element_id es el identificador del recurso al cual se esta subscrito.
    // - value: el value del campo contiene la informacion necesaria para realizar la notificacion
    // via HTTP Callback (estos datos son el endpoint, la secretKey a utilizar, politica de
    // reintentos, ...)

    final Topic topic = ChannelUtils.getChannel(subscription);

    // Habilitamos listener
    activateSubscription(subscription.getSourceEntityId(), topic, subscription.getNotificationParams());

    // Persistimos en Redis la subscripcion
    sRedisTemplate.hSet(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic(),
        converter.marshal(subscription.getNotificationParams()));

    LOGGER.info("Listener {} subscribed to channel {}", subscription.getSourceEntityId(), topic.getTopic());
  }

  @Override
  public void remove(final Subscription subscription) {
    // Al eliminar una subscripcion, no solo se deberá eliminar la relación listener - topic del
    // contenedor de listeners, sino que también se deberá eliminar la subscripcion de este
    // listener a este canal en Redis.

    // Este metodo puede invocarse para:
    // 1. eliminar una subscripcion en concreto (hdel pasando un field)
    // 2. todas las subscripciones de un tipo (hdel pasando más de un field)
    // 3. todas las subscripciones (de la entidad identificada por la key)
    // En cada caso el comportamiento es diferente.

    if (subscription.getType() == null) {
      removeAllSubscriptions(subscription);
    } else if (subscription.hasResourceIdentified()) {
      removeSubscription(subscription);
    } else {
      removeSubscriptionsOfOneType(subscription);
    }
  }

  @Override
  public List<Subscription> get(final Subscription subscription) {
    // Las subscripciones de una entidad estan registradas bajo la clave subs:idEntity en Redis
    // El valor asociado a la clave es una hash de pares <channel, notificationParam>
    // donde cada channel representa una subscripción activa de esa entidad.

    LOGGER.debug("Retrieving active subscriptions for entity {}", subscription.getSourceEntityId());

    final String key = keysBuilder.getSubscriptionKey(subscription.getSourceEntityId());
    final Map<String, String> subscriptions = sRedisTemplate.hGetAll(key);
    final List<Subscription> subscriptionList = buildEntitySubscriptions(subscription, subscriptions);

    LOGGER.debug("Entity {} has {} active subscriptions", subscription.getSourceEntityId(), subscriptionList.size());

    return subscriptionList;
  }

  private void checkTargetResourceState(final Subscription subscription) {
    switch (subscription.getType()) {
      case DATA:
        checkSensorState(((DataSubscription) subscription).getProviderId(), ((DataSubscription) subscription).getSensorId());
        break;
      case ORDER:
        checkSensorState(((OrderSubscription) subscription).getOwnerEntityId(), ((OrderSubscription) subscription).getSensorId());
        break;
      case ALARM:
        checkAlertState(((AlarmSubscription) subscription).getAlertId());
        break;
      default:
        throw new IllegalArgumentException("Unknown subscription type:" + subscription.getType());
    }
  }

  private void checkSensorState(final String providerId, final String sensorId) {
    if (!StringUtils.hasText(sensorId)) {
      return;
    }

    final SensorState sensorState = resourceService.getSensorState(providerId, sensorId);
    final boolean existsSensor = sensorState != null && !sensorState.equals(SensorState.ghost);

    if (!existsSensor) {
      throw new ResourceNotFoundException(sensorId, "Sensor");
    } else if (SensorState.offline.equals(sensorState)) {
      throw new ResourceOfflineException(sensorId, "Sensor");
    }
  }

  private void checkAlertState(final String alertId) {
    if (StringUtils.hasText(alertId) && !resourceService.existsAlert(alertId)) {
      throw new ResourceNotFoundException(alertId, "Alert");
    } else if (StringUtils.hasText(alertId) && resourceService.isAlertDisabled(alertId)) {
      throw new ResourceOfflineException(alertId, "Alert");
    }
  }

  private void activateStoredSubscriptions(final Set<String> storedSubscriptions) {
    // Patch!!: if there are not stored subscriptions in Redis, a default listener is initialized,
    // subscribed to a mock channel.
    // This approach allows to evict the error " ERR wrong number of arguments for 'psubscribe'
    // command;" thrown by the listener container when no exist subscriptions
    if (CollectionUtils.isEmpty(storedSubscriptions)) {
      doRegisterMockSubscription();
      return;
    }

    LOGGER.info("Found {} subscriptions stored in Redis", storedSubscriptions.size());

    for (final String subscriptionKey : storedSubscriptions) {
      // Each subscriptionKey represents an entity subscribed to N channels (in Redis is stored as a
      // hash).
      // For each hash entry, the key is the channel and the value stores the notification params.
      final Map<String, String> storedSubscription = sRedisTemplate.hGetAll(subscriptionKey);
      if (CollectionUtils.isEmpty(storedSubscription)) {
        return;
      }

      final Set<String> channels = storedSubscription.keySet();
      final String listenerName = listenerNameFromSubscriptionKey(subscriptionKey);

      for (final String channel : channels) {
        final NotificationParams notifParams = (NotificationParams) converter.unmarshal(storedSubscription.get(channel), NotificationParams.class);
        activateSubscription(listenerName, ChannelUtils.buildTopic(channel), notifParams);
      }
    }
  }

  private String listenerNameFromSubscriptionKey(final String subscriptionKey) {
    // subscriptionKey follows the expression subs:<listenerName>
    final int pos = subscriptionKey.lastIndexOf(PubSubConstants.REDIS_KEY_TOKEN);
    return subscriptionKey.substring(pos + 1);
  }

  private void activateSubscription(final String listenerName, final Topic topic, final NotificationParams notificationParams) {
    MessageListenerImpl listener = listeners.get(listenerName);
    if (listener == null) {
      listener = addNewListener(listenerName);
    }

    LOGGER.info("Subscribing listener {} to channel {}", listener.getName(), topic.getTopic());

    listenerContainer.addMessageListener(listener, topic);
    listener.addSubscription(topic, notificationParams);
  }

  private MessageListenerImpl addNewListener(final String listenerName) {
    MessageListenerImpl listener = null;
    try {
      listenerFactory.setListenerName(listenerName);
      listener = listenerFactory.getObject();
      listeners.put(listener.getName(), listener);
    } catch (final Exception e) {
      LOGGER.warn("Has been unable to build MessageListener {}. ", listenerName, e);
    }
    return listener;

  }

  private void doRegisterMockSubscription() {
    LOGGER.debug("Not found stored subscriptions in Redis. Registering a mock subscription to channel {}", DUMMY_TOPIC);
    final MessageListenerImpl listener = new MockMessageListenerImpl(DUMMY_TOPIC);
    listenerContainer.addMessageListener(listener, ChannelUtils.buildTopic(DUMMY_TOPIC));
  }

  private void removeSubscription(final Subscription subscription) {
    final Topic topic = ChannelUtils.getChannel(subscription);

    LOGGER.debug("Removing subscription to channel {} for listener {} ", topic.getTopic(), subscription.getSourceEntityId());

    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
    if (listener != null) {
      listenerContainer.removeMessageListener(listener, topic);
      listener.removeSubscription(topic);
    }

    // Finally, the subscription to the topic is removed from Redis
    sRedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topic.getTopic());

    LOGGER.debug("Removed subscription from listener {} to channel {}", subscription.getSourceEntityId(), topic.getTopic());
  }

  private void removeAllSubscriptions(final Subscription subscription) {
    LOGGER.debug("Removing all subscriptions for listener {}", subscription.getSourceEntityId());

    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
    if (listener != null) {
      listenerContainer.removeMessageListener(listener);
      // listener is removed from the active listeners list
      listeners.remove(subscription.getSourceEntityId());
    }

    // Finally, the subscription is removed from Redis
    sRedisTemplate.del(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));

    LOGGER.debug("Subscriptions removed for listener {} ", subscription.getSourceEntityId());
  }

  private void removeSubscriptionsOfOneType(final Subscription subscription) {
    // Por ejemplo, puede ser borrar todas las subscripciones de tipo alarm del listener.
    // Esto implica en Redis recuperar primero todas las subscripciones de ese tipo y despues borrar
    // via un hdel
    // En el container se trata de recuperar todas las subscripciones que existen (holders) y para
    // cada una, si se trata de una subscripcion del tipo indicado se pasa a eliminar el listener
    // de su lista de listeners.

    LOGGER.debug("Removing all subscriptions of type {} for listener {}", subscription.getType(), subscription.getSourceEntityId());

    // Recuperamos todos los canales a los cuales esta subscrito el listener.
    final Set<String> topics = sRedisTemplate.hKeys(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()));
    final List<String> topicsToRemove = ChannelUtils.filterTopicsOfType(topics, subscription.getType());

    // Y para cada uno, eliminamos la subscripcion activa del listener
    final MessageListenerImpl listener = listeners.get(subscription.getSourceEntityId());
    for (final String topicName : topicsToRemove) {
      if (listener != null) {
        final Topic topic = ChannelUtils.buildTopic(topicName);
        listenerContainer.removeMessageListener(listener, topic);
        listener.removeSubscription(topic);
      }
    }

    LOGGER.debug("Number of subscriptions to remove in Redis: {}", topicsToRemove.size());

    if (!CollectionUtils.isEmpty(topicsToRemove)) {
      // Por ultimo, eliminamos en Redis de la hash de subscripciones todas aquellas que
      // corresponden a canales del tipo indicado:
      sRedisTemplate.hDel(keysBuilder.getSubscriptionKey(subscription.getSourceEntityId()), topicsToRemove.toArray(new String[0]));
    }

    LOGGER.debug("Subscriptions of type {} removed for listener {} ", subscription.getType(), subscription.getSourceEntityId());
  }

  private List<Subscription> buildEntitySubscriptions(final Subscription subscription, final Map<String, String> subscriptions) {
    final List<Subscription> subscriptionList = new ArrayList<Subscription>();
    if (!CollectionUtils.isEmpty(subscriptions)) {
      final Iterator<String> it = subscriptions.keySet().iterator();
      final SubscribeType filterType = subscription.getType();
      while (it.hasNext()) {
        final String field = it.next();
        if (filterType == null || ChannelUtils.isTopicOfType(field, filterType)) {
          subscriptionList.add(ChannelUtils.getSubscription(subscription.getSourceEntityId(), field, subscriptions.get(field)));
        }
      }
    }

    return subscriptionList;
  }

}
