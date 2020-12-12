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
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.RejectedResourcesContext;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.service.AlarmService;
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
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class AlarmServiceImpl extends AbstractPlatformServiceImpl implements AlarmService {

  private static final Logger LOGGER = LoggerFactory.getLogger(AlarmServiceImpl.class);

  @Autowired
  private ResourceService resourceService;

  private final LRUCache<String, String> alertsOwners = new LRUCacheImpl<String, String>(1000, 15);

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.AlarmService#setAlarm(org.sentilo.platform.common.domain
   * .AlarmInputMessage)
   */
  @Metric(requestType = RequestType.PUT, eventType = EventType.ALARM)
  public void setAlarm(final AlarmInputMessage message) {
    final RejectedResourcesContext rejectedContext = new RejectedResourcesContext();

    try {
      checkTargetResourceState(message);

      LOGGER.debug("Set alarm [{}] from alert [{}] ", message.getMessage(), message.getAlertId());
      // Save alarm in Redis
      registerAlarmMessage(message);
      // and finally, publish the alarm
      publish(message);
    } catch (final ResourceNotFoundException rnfe) {
      rejectedContext.rejectEvent(message.getAlertId(), rnfe.getMessage());
      LOGGER.warn("Alarm [{}] has been rejected because alert [{}] doesn't exists on Sentilo.", message.getMessage(), message.getAlertId());
    } catch (final ResourceOfflineException roe) {
      rejectedContext.rejectEvent(message.getSensorId(), roe.getMessage());
      LOGGER.warn("Alarm [{}] has been rejected because alert [{}] is not enabled on Sentilo.", message.getMessage(), message.getAlertId());
    }

    if (!rejectedContext.isEmpty()) {
      throw new EventRejectedException(EventType.ALARM, rejectedContext);
    }

  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.AlarmService#getAlertOwner(java.lang.String)
   */
  public String getAlertOwner(final String alertId) {
    final String alertOwner = getOwnerFromCache(alertId);
    if (alertOwner == null) {
      throw new ResourceNotFoundException(alertId, "Alert");
    }
    return alertOwner;
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
    final Long aid = sequenceUtils.getAid(message.getAlertId());
    if (aid != null) {
      messages.addAll(getLastAlarms(aid, message));
    }

    return messages;
  }

  private String getOwnerFromCache(final String alertId) {
    if (alertsOwners.get(alertId) == null) {
      final Long aid = sequenceUtils.getAid(alertId);
      final Alert alert = resourceService.getAlert(aid);
      if (alert != null) {
        alertsOwners.put(alertId, alert.getEntity());
      }
    }

    return alertsOwners.get(alertId);
  }

  /**
   * If alert has filled in the alertId, checks if it exists and is enabled in Redis. Otherwise
   * throws an exception.
   */
  private void checkTargetResourceState(final AlarmInputMessage message) {
    if (!resourceService.existsAlert(message.getAlertId())) {
      throw new ResourceNotFoundException(message.getAlertId(), "Alert");
    } else if (resourceService.isAlertDisabled(message.getAlertId())) {
      throw new ResourceOfflineException(message.getAlertId(), "Alert");
    }
  }

  private Long registerAlarmMessage(final AlarmInputMessage message) {
    final Long aid = sequenceUtils.getAid(message.getAlertId());
    final Long amid = sequenceUtils.getAmid();
    final Long timestamp = System.currentTimeMillis();
    final String alarmKey = keysBuilder.getAlarmKey(amid);

    // Build a new hash with key amid:{amid} and values source, message, timestamp and alertType.
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put(AID, Long.toString(aid));
    fields.put(SENDER, message.getSender());
    fields.put(MESSAGE, message.getMessage());
    fields.put(TIMESTAMP, timestamp.toString());
    if (StringUtils.hasText(message.getAlertType())) {
      fields.put(ALERT_TYPE, message.getAlertType());
    }

    sRedisTemplate.hmSet(alarmKey, fields);

    // if expireSeconds is defined and !=0, set the expire time to key
    if (expireSeconds != 0) {
      sRedisTemplate.expire(alarmKey, expireSeconds);
    }

    // Finalmente, definimos una reverse lookup key con la cual recuperar rapidamente las alarmas de
    // una alerta
    // A continuacion, añadimos el amid al Sorted Set alert:{aid}:messages. La puntuacion, o score,
    // que se asocia a cada elemento del Set es el timestamp del mensaje.
    sRedisTemplate.zAdd(keysBuilder.getAlertAlarmsKey(aid), timestamp, amid.toString());

    LOGGER.debug("Registered in Redis alarm [{}] from alert [{}]", amid, message.getAlertId());

    return amid;
  }

  private void publish(final AlarmInputMessage message) {
    LOGGER.debug("Publish alarm event message [{}] associated with alert [{}]", message.getMessage(), message.getAlertId());
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlertId());
    sRedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(message, topic));
  }

  private List<Alarm> getLastAlarms(final Long aid, final AlarmInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // La sentencia a utilizar en Redis es:
    // ZREVRANGEBYSCORE aid:{aid}:alarms to from LIMIT 0 limit

    final Set<String> amids = sRedisTemplate.zRevRangeByScore(keysBuilder.getAlertAlarmsKey(aid), to, from, 0, limit);
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

  private Alarm getAlarm(final Long amid, final String alertId) {
    Alarm alarm = null;
    String message = null;
    String ts = null;
    String sender = null;

    final Map<String, String> infoSoid = sRedisTemplate.hGetAll(keysBuilder.getAlarmKey(amid));
    if (!CollectionUtils.isEmpty(infoSoid)) {
      message = infoSoid.get(MESSAGE);
      ts = infoSoid.get(TIMESTAMP);
      sender = infoSoid.get(SENDER);

      alarm = new Alarm(alertId, message, sender, Long.parseLong(ts));
    }

    return alarm;
  }

}
