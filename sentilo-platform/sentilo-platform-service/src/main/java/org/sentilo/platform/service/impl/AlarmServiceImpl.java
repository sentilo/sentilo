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
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.common.domain.AlertOwner;
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.common.service.ResourceService;
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

@Service
public class AlarmServiceImpl extends AbstractPlatformServiceImpl implements AlarmService {

  private final Logger logger = LoggerFactory.getLogger(AlarmServiceImpl.class);

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
  public void setAlarm(final AlarmInputMessage message) {
    // Si la alarma no existe, la registramos en Redis
    registerAlarmIfNecessary(message);
    // Registramos en Redis el mensaje asociado a la alarma
    registerAlarmMessage(message);
    // Y por ultimo, publicamos el mensaje de la alarma
    publish(message);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.platform.common.service.AlarmService#getAlarmOwner(java.lang.String)
   */
  public String getAlertOwner(final String alertId) {
    return alertsOwners.get(alertId);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.common.service.AlarmService#getLastMessages(org.sentilo.platform.common
   * .domain.AlarmInputMessage)
   */
  public List<Alarm> getLastMessages(final AlarmInputMessage message) {
    // Para recuperar los mensajes asociados a una alarma, debemos hacer lo siguiente:
    // 1. Recuperar el identificador interno de la alarma en Redis
    // 2. Si este no es null, recuperar los mensajes asociados a esta alarma que cumplen el criterio
    // de busqueda
    final List<Alarm> messages = new ArrayList<Alarm>();
    final Long aid = jedisSequenceUtils.getAid(message.getAlertId());
    if (aid != null) {
      messages.addAll(getLastAlarms(aid, message));
    }

    return messages;
  }

  private void registerAlarmIfNecessary(final AlarmInputMessage message) {
    // Si la alarma aun no esta registrada en Redis, la registramos.
    resourceService.registerAlarmIfNecessary(message.getAlertId());
  }

  private void registerAlarmMessage(final AlarmInputMessage message) {
    final Long aid = jedisSequenceUtils.getAid(message.getAlertId());
    final Long amid = persistAlarmMessage(message);
    registerAlarmMessage(aid, amid, message);
  }

  private void registerAlarmMessage(final Long aid, final Long amid, final AlarmInputMessage message) {
    final Long timestamp = System.currentTimeMillis();
    // Definimos una reverse lookup key con la cual recuperar rapidamente los mensajes de una alarma
    // A continuacion, añadimos el amid al Sorted Set alarm:{aid}:messages. La puntuacion, o score,
    // que se asocia
    // a cada elemento del Set es el timestamp del mensaje.
    jedisTemplate.zAdd(keysBuilder.getAlarmMessagesKey(aid), timestamp, amid.toString());

    logger.debug("Registered in Redis message {} for alarm {}", amid, message.getAlertId());
  }

  private Long persistAlarmMessage(final AlarmInputMessage message) {
    final Long amid = jedisSequenceUtils.getAmid();

    final Long timestamp = System.currentTimeMillis();

    final String alarmKey = keysBuilder.getMessageKey(amid);

    // Guardamos una hash de clave amid:{amid} y valores source, message y timestamp.
    final Map<String, String> fields = new HashMap<String, String>();
    fields.put(SENDER, message.getSender());
    fields.put(MESSAGE, message.getMessage());
    fields.put(TIMESTAMP, timestamp.toString());
    jedisTemplate.hmSet(alarmKey, fields);

    // if expireSeconds is defined and !=0, set the expire time to key
    if (expireSeconds != 0) {
      jedisTemplate.expire(alarmKey, expireSeconds);
    }

    return amid;
  }

  private void publish(final AlarmInputMessage message) {
    logger.debug("Publish alarm event message {} associated with alarm {}", message.getMessage(), message.getAlertId());
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlertId());
    jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(message, topic));
  }

  private List<Alarm> getLastAlarms(final Long aid, final AlarmInputMessage message) {
    final Long to = QueryFilterParamsUtils.getTo(message);
    final Long from = QueryFilterParamsUtils.getFrom(message);
    final Integer limit = QueryFilterParamsUtils.getLimit(message);

    // La sentencia a utilizar en Redis es:
    // ZREVRANGEBYSCORE aid:{aid}:messages to from LIMIT 0 limit

    final Set<String> amids = jedisTemplate.zRevRangeByScore(keysBuilder.getAlarmMessagesKey(aid), to, from, 0, limit);
    final List<Alarm> alarmMessages = (!CollectionUtils.isEmpty(amids) ? getAlarms(amids, message.getAlertId()) : Collections.<Alarm>emptyList());

    return alarmMessages;
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

    final Map<String, String> infoSoid = jedisTemplate.hGetAll(keysBuilder.getMessageKey(amid));
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
      logger.debug("Actualizando cache de propietarios de alertas");
      final List<AlertOwner> owners = catalogService.getAlertsOwners().getOwners();
      final Map<String, String> auxAlertsOwners = new HashMap<String, String>();
      if (!CollectionUtils.isEmpty(owners)) {
        for (final AlertOwner alarmOwner : owners) {
          auxAlertsOwners.put(alarmOwner.getAlertId(), alarmOwner.getOwnerEntityId());
        }
      }

      replaceActiveAlertsOwners(auxAlertsOwners);

    } catch (final CatalogAccessException e) {
      logger.warn("Error al llamar al catalogo para recuperar la lista de propietarios de alarmas", e);
    }
  }

  private void replaceActiveAlertsOwners(final Map<String, String> updatedAlertsOwners) {
    alertsOwners.clear();
    alertsOwners.putAll(updatedAlertsOwners);
  }
}
