/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.AlertOwner;
import org.sentilo.common.utils.EventType;
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.monitor.Metric;
import org.sentilo.platform.service.monitor.RequestType;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.sentilo.platform.service.utils.QueryFilterParamsUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class AlarmServiceImpl extends AbstractPlatformServiceImpl implements AlarmService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AlarmServiceImpl.class);

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ResourceService resourceService;

  private final Map<String, String> alertsOwners = new HashMap<String, String>();

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.common.service.AlarmService#setAlarm(org.sentilo.platform.common.domain
   * .AlarmInputMessage)
   */
  @Metric(requestType = RequestType.PUT, eventType = EventType.ALARM)
  public void setAlarm(final AlarmInputMessage message) {
    // Register alert in Redis if it does not exists
    registerAlertIfNeedBe(message);
    // Add alarm in Redis
    registerAlarmMessage(message);
    // and finally, publish the alarm
    publish(message);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.common.service.AlarmService#getAlertOwner(java.lang.String)
   */
  public String getAlertOwner(final String alertId) throws ResourceNotFoundException {
    if (alertsOwners.get(alertId) == null) {
      throw new ResourceNotFoundException(alertId, "Alert");
    }
    return alertsOwners.get(alertId);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.common.service.AlarmService#getLastAlarms(org.sentilo.platform.common.
   * domain.AlarmInputMessage)
   */
  @Metric(requestType = RequestType.GET, eventType = EventType.ALARM)
  public List<Alarm> getLastAlarms(final AlarmInputMessage message) {
    // Para recuperar las alarmas asociadas a una alerta, debemos hacer lo siguiente:
    // 1. Recuperar el identificador interno de la alerta en Redis
    // 2. Si este no es null, recuperar las alarmas asociadas a esta alerta que cumplen el criterio
    // de busqueda
    final List<Alarm> messages = new ArrayList<Alarm>();
    final Long aid = jedisSequenceUtils.getAid(message.getAlertId());
    if (aid != null) {
      messages.addAll(getLastAlarms(aid, message));
    }

    return messages;
  }

  private void registerAlertIfNeedBe(final AlarmInputMessage message) {
    resourceService.registerAlertIfNeedBe(message.getAlertId());
  }

  private void registerAlarmMessage(final AlarmInputMessage message) {
    final Long aid = jedisSequenceUtils.getAid(message.getAlertId());
    final Long amid = persistAlarmMessage(message);
    registerAlarmMessage(aid, amid, message);
  }

  private void registerAlarmMessage(final Long aid, final Long amid, final AlarmInputMessage message) {
    final Long timestamp = System.currentTimeMillis();
    // Definimos una reverse lookup key con la cual recuperar rapidamente las alarmas de una alerta
    // A continuacion, añadimos el amid al Sorted Set alert:{aid}:messages. La puntuacion, o score,
    // que se asocia
    // a cada elemento del Set es el timestamp del mensaje.
    jedisTemplate.zAdd(keysBuilder.getAlertAlarmsKey(aid), timestamp, amid.toString());

    LOGGER.debug("Registered in Redis alarm {} for alert {}", amid, message.getAlertId());
  }

  private Long persistAlarmMessage(final AlarmInputMessage message) {
    final Long amid = jedisSequenceUtils.getAmid();

    final Long timestamp = System.currentTimeMillis();

    final String alarmKey = keysBuilder.getAlarmKey(amid);

    // Build a new hash with key amid:{amid} and values source, message, timestamp and alertType.
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put(SENDER, message.getSender());
    fields.put(MESSAGE, message.getMessage());
    fields.put(TIMESTAMP, timestamp.toString());
    if (StringUtils.hasText(message.getAlertType())) {
      fields.put(ALERT_TYPE, message.getAlertType());
    }

    jedisTemplate.hmSet(alarmKey, fields);

    // if expireSeconds is defined and !=0, set the expire time to key
    if (expireSeconds != 0) {
      jedisTemplate.expire(alarmKey, expireSeconds);
    }

    return amid;
  }

  private void publish(final AlarmInputMessage message) {
    LOGGER.debug("Publish alarm event message {} associated with alert {}", message.getMessage(), message.getAlertId());
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlertId());
    jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(message, topic));
  }

  private List<Alarm> getLastAlarms(final Long aid, final AlarmInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // La sentencia a utilizar en Redis es:
    // ZREVRANGEBYSCORE aid:{aid}:alarms to from LIMIT 0 limit

    final Set<String> amids = jedisTemplate.zRevRangeByScore(keysBuilder.getAlertAlarmsKey(aid), to, from, 0, limit);
    return !CollectionUtils.isEmpty(amids) ? getAlarms(amids, message.getAlertId()) : Collections.<Alarm>emptyList();
  }

  private List<Alarm> getAlarms(final Set<String> amids, final String alarmId) {
    final List<Alarm> alarmMessages = new ArrayList<Alarm>();
    final Iterator<String> it = amids.iterator();

    while (it.hasNext()) {
      final Long amid = Long.parseLong(it.next());
      final Alarm alarm = getAlarm(amid, alarmId);
      if (alarm != null) {
        alarmMessages.add(alarm);
      }
    }
    return alarmMessages;
  }

  private Alarm getAlarm(final Long amid, final String alarmId) {
    Alarm alarm = null;
    String message = null;
    String ts = null;
    String sender = null;

    final Map<String, String> infoSoid = jedisTemplate.hGetAll(keysBuilder.getAlarmKey(amid));
    if (!CollectionUtils.isEmpty(infoSoid)) {
      message = infoSoid.get(MESSAGE);
      ts = infoSoid.get(TIMESTAMP);
      sender = infoSoid.get(SENDER);

      alarm = new Alarm(alarmId, message, sender, Long.parseLong(ts));
    }

    return alarm;
  }

  @Scheduled(initialDelay = 5000, fixedRate = 900000)
  public void loadAlertsOwners() {
    try {
      LOGGER.debug("Updating alert owners cache");
      final List<AlertOwner> owners = catalogService.getAlertsOwners().getOwners();
      final Map<String, String> auxAlertsOwners = new HashMap<String, String>();
      if (!CollectionUtils.isEmpty(owners)) {
        for (final AlertOwner alarmOwner : owners) {
          auxAlertsOwners.put(alarmOwner.getAlertId(), alarmOwner.getOwnerEntityId());
        }
      }

      replaceActiveAlertsOwners(auxAlertsOwners);

    } catch (final CatalogAccessException e) {
      LOGGER.warn("Error calling the catalog for get the list of alert owners", e);
    }
  }

  private void replaceActiveAlertsOwners(final Map<String, String> updatedAlertsOwners) {
    alertsOwners.clear();
    alertsOwners.putAll(updatedAlertsOwners);
  }
}
