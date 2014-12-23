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
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.service.OrderService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.sentilo.platform.service.utils.QueryFilterParamsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class OrderServiceImpl extends AbstractPlatformServiceImpl implements OrderService {

  private final Logger logger = LoggerFactory.getLogger(OrderServiceImpl.class);

  @Autowired
  private ResourceService resourceService;

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.common.service.OrderService#setOrder(org.sentilo.platform.common.domain
   * .OrderInputMessage)
   */
  public void setOrder(final OrderInputMessage message) {
    logger.debug("set order {} for provider {} and sensor {} ", message.getOrder(), message.getProviderId(), message.getSensorId());
    // Si el proveedor y/o el sensor no existen, los registramos en Redis
    registerProviderAndSensorIfNecessary(message);
    // Registramos en Redis la orden
    registerOrder(message);
    // Y por ultimo, publicamos la orden
    publish(message);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.common.service.OrderService#getLastOrders(org.sentilo.platform.common.
   * domain.OrderInputMessage)
   */
  public List<Order> getLastOrders(final OrderInputMessage message) {
    // Para recuperar las ordenes del sensor / sensores de un proveedor, debemos hacer lo siguiente:
    // 1. Recuperar los identificadores internos de los sensores de los cuales queremos recuperar
    // las
    // ordenes.
    // 2. Para cada sensor, recuperar las ordenes que cumplen el criterio de busqueda
    final List<Order> globalOrders = new ArrayList<Order>();
    final Set<String> sids = resourceService.getSensorsToInspect(message.getProviderId(), message.getSensorId());
    if (CollectionUtils.isEmpty(sids)) {
      logger.debug("Provider {} has not sensors registered", message.getProviderId());
      return globalOrders;
    }

    logger.debug("Retrieving last orders for {} sensors of provider {}", sids.size(), message.getProviderId());

    final Iterator<String> it = sids.iterator();
    while (it.hasNext()) {
      final Long sid = Long.parseLong(it.next());
      final List<Order> ordersFromSensor = getLastOrders(sid, message);
      if (!CollectionUtils.isEmpty(ordersFromSensor)) {
        globalOrders.addAll(ordersFromSensor);
      }
    }

    return globalOrders;
  }

  private List<Order> getLastOrders(final Long sid, final OrderInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // La sentencia a utilizar en Redis es:
    // ZREVRANGEBYSCORE sid:{sid}:orders to from LIMIT 0 limit
    final Sensor sensor = resourceService.getSensor(sid);
    final Set<String> soids = jedisTemplate.zRevRangeByScore(keysBuilder.getSensorOrdersKey(sid), to, from, 0, limit);
    List<Order> orders = null;

    if (!CollectionUtils.isEmpty(soids)) {
      orders = getOrders(sensor, soids);
    }

    return orders;
  }

  private List<Order> getOrders(final Sensor sensor, final Set<String> soids) {
    final List<Order> orders = new ArrayList<Order>();
    final Iterator<String> it = soids.iterator();

    while (it.hasNext()) {
      final Long soid = Long.parseLong(it.next());
      final Order order = getOrder(soid);
      if (order != null) {
        order.setProvider(sensor.getProvider());
        order.setSensor(sensor.getSensor());
        orders.add(order);
      }
    }
    return orders;
  }

  private Order getOrder(final Long soid) {
    Order order = null;
    String message = null;
    String ts = null;
    String sender = null;

    final Map<String, String> infoSoid = jedisTemplate.hGetAll(keysBuilder.getOrderKey(soid));
    if (!CollectionUtils.isEmpty(infoSoid)) {
      message = infoSoid.get(ORDER);
      ts = infoSoid.get(TIMESTAMP);
      sender = infoSoid.get(SENDER);

      order = new Order(message, sender, Long.parseLong(ts));
    }

    return order;
  }

  private void registerProviderAndSensorIfNecessary(final OrderInputMessage message) {
    // Si el proveedor y/o el sensor aun no estan registrados en Redis, los registramos.
    resourceService.registerProviderIfNecessary(message.getProviderId());
    if (StringUtils.hasText(message.getSensorId())) {
      resourceService.registerSensorIfNecessary(message.getSensorId(), message.getProviderId());
    }
  }

  private void registerOrder(final OrderInputMessage message) {
    // En funcion del destinatario de la orden, debemos persistirla para 1 o N sensores
    logger.debug("registering in Redis order {} ", message.getOrder());
    if (StringUtils.hasText(message.getSensorId())) {
      registerSensorOrder(message);
    } else {
      registerProviderOrder(message);
    }
    logger.debug("Order registered ");
  }

  private void registerProviderOrder(final OrderInputMessage message) {
    final Set<String> sids = resourceService.getSensorsFromProvider(message.getProviderId());

    if (CollectionUtils.isEmpty(sids)) {
      logger.debug("Provider {} has not sensors registered", message.getProviderId());
      return;
    }

    logger.debug("Found {} sensors registered for provider {}", sids.size(), message.getProviderId());

    final Long soid = persistOrder(message);
    final Iterator<String> it = sids.iterator();
    while (it.hasNext()) {
      final String sid = it.next();
      registerSensorOrder(new Long(sid), soid, message);
    }
  }

  private void registerSensorOrder(final OrderInputMessage message) {
    final Long sid = jedisSequenceUtils.getSid(message.getProviderId(), message.getSensorId());
    final Long soid = persistOrder(message);
    registerSensorOrder(sid, soid, message);
  }

  private void registerSensorOrder(final Long sid, final Long soid, final OrderInputMessage message) {
    final Long timestamp = System.currentTimeMillis();
    // Definimos una reverse lookup key con la cual recuperar rapidamente las ordenes de un sensor
    // A continuacion, añadimos el soid al Sorted Set sensor:{sid}:orders. La puntuacion, o score,
    // que se asocia
    // a cada elemento del Set es el timestamp de la orden.
    jedisTemplate.zAdd(keysBuilder.getSensorOrdersKey(sid), timestamp, soid.toString());

    logger.debug("Registered in Redis order {} for sensor {} from provider {}", soid, message.getSensorId(), message.getProviderId());
  }

  private Long persistOrder(final OrderInputMessage message) {
    final Long soid = jedisSequenceUtils.getSoid();

    final Long timestamp = System.currentTimeMillis();

    final String orderKey = keysBuilder.getOrderKey(soid);

    // Guardamos una hash de clave soid:{soid} y valores sender, order y timestamp.
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put(SENDER, message.getSender());
    fields.put(ORDER, message.getOrder());
    fields.put(TIMESTAMP, timestamp.toString());
    jedisTemplate.hmSet(orderKey, fields);

    // if expireSeconds is defined and !=0, set the expire time to key
    if (expireSeconds != 0) {
      jedisTemplate.expire(orderKey, expireSeconds);
    }

    return soid;
  }

  private void publish(final OrderInputMessage message) {
    // Y luego la publicamos.
    logger.debug("Publish order event {} related to provider {} and sensor {}", message.getOrder(), message.getProviderId(), message.getSensorId());
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.order, message.getProviderId(), message.getSensorId());
    jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(message, topic));
    logger.debug("Order published");
  }
}
