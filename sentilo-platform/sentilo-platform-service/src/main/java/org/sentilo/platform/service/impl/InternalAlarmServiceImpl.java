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

import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.cache.impl.LRUCacheImpl;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.repository.EntityMetadataRepository;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Component;

@Component
public class InternalAlarmServiceImpl implements InternalAlarmService {

  private static final Logger LOGGER = LoggerFactory.getLogger(InternalAlarmService.class);
  private static final String INTERNAL_TYPE = "INTERNAL";

  private static final String GHOST_MESSAGE_TEMPLATE = "Detected ghost sensor %s belonging to provider %s";
  private static final String GLOBAL_INBOUND_RL_MESSAGE_TEMPLATE =
      "Entity %s has exceeded the global inbound quota of %s requests by hour. Remaining time to allow new requests: %s minutes";
  private static final String INBOUND_RL_MESSAGE_TEMPLATE =
      "Entity %s has exceeded its quota of %s requests by hour. Remaining time to allow new requests: %s minutes";
  private static final String OUTBOUND_RL_MESSAGE_TEMPLATE =
      "Entity %s has exceeded its quota of %s notifications by hour. Remaining time to allow new notifications: %s minutes";

  @Autowired
  private StringRedisTemplate redisTemplate;

  @Autowired
  private EntityMetadataRepository entityMetadataRepository;

  /** Internal cache to evict spam with ghost alarms notifications. */
  private final LRUCache<String, String> ghostSensors = new LRUCacheImpl<String, String>(1000, 10);

  /**
   * Internal cache to evict spam with over_quota alarms notifications: only one over_quota alarm
   * must be sent each minute.
   */
  private final LRUCache<String, Long> inboundOverQuotaEntities = new LRUCacheImpl<String, Long>(1000, 1);
  private final LRUCache<String, Long> outboundOverQuotaEntities = new LRUCacheImpl<String, Long>(1000, 1);

  @Override
  public void publishGhostSensorAlarm(final Observation data) {
    final String ghostSensorKey = data.getProvider() + "." + data.getSensor();
    if (ghostSensors.get(ghostSensorKey) == null) {
      final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, data.getProvider(), data.getSensor());

      final AlarmInputMessage aim = new AlarmInputMessage();
      aim.setProviderId(data.getProvider());
      aim.setSensorId(data.getSensor());
      aim.setAlertType(INTERNAL_TYPE);
      aim.setAlertId(SentiloConstants.GHOST_SENSOR_ALERT);
      aim.setSender(SentiloConstants.SENTILO_SENDER);
      aim.setMessage(String.format(GHOST_MESSAGE_TEMPLATE, data.getSensor(), data.getProvider()));

      redisTemplate.convertAndSend(topic.getTopic(), PublishMessageUtils.buildContentToPublish(aim, topic));
      ghostSensors.put(ghostSensorKey, aim.getMessage());
      LOGGER.info("Published new ghost sensor alarm related to sensor [{}] from provider [{}]", data.getSensor(), data.getProvider());
    }
  }

  @Override
  public void publishInboundRateLimiterAlarm(final String entity, final QuotaContext quotaContext) {
    if (inboundOverQuotaEntities.get(entity) == null) {
      final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, entity);
      final String oq_message =
          String.format(getInboundMessageTemplate(quotaContext), entity, quotaContext.getLimit(), quotaContext.getMinutesToReset());

      final AlarmInputMessage aim = new AlarmInputMessage();
      aim.setAlertType(INTERNAL_TYPE);
      aim.setAlertId(getInboundAlertId(quotaContext));
      aim.setSender(SentiloConstants.SENTILO_SENDER);
      aim.setMessage(oq_message);

      redisTemplate.convertAndSend(topic.getTopic(), PublishMessageUtils.buildContentToPublish(aim, topic));
      inboundOverQuotaEntities.put(entity, System.currentTimeMillis());
      LOGGER.info("Published new over quota inbound alarm related to entity [{}]: {}  ", entity, oq_message);
    }
  }

  @Override
  public void publishOutboundRateLimiterAlarm(final String entity, final QuotaContext quotaContext) {
    if (outboundOverQuotaEntities.get(entity) == null) {
      // final String oq_message_template = "Entity %s has exceeded its quota of %s notifications by
      // hour. Remaining time to allow new notifications: %s minutes";
      final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, entity);
      final String oq_message = String.format(OUTBOUND_RL_MESSAGE_TEMPLATE, entity, quotaContext.getLimit(), quotaContext.getMinutesToReset());

      final AlarmInputMessage aim = new AlarmInputMessage();
      aim.setAlertType(INTERNAL_TYPE);
      aim.setAlertId(SentiloConstants.OVER_QUOTA_OUTBOUND_ALERT);
      aim.setSender(SentiloConstants.SENTILO_SENDER);
      aim.setMessage(oq_message);

      try {
        // To fill in additional fields, PublishMessageUtils need to get
        // RequesterContextHolder.getContext() which holds
        // this information. As Notification flow runs async, RequesterContextHolder.getContext() is
        // null and need to be
        // generate before invoke method.
        final RequesterContext rc = new RequesterContext(entityMetadataRepository.getEntityMetadataFromId(entity));
        RequesterContextHolder.setContext(rc);

        redisTemplate.convertAndSend(topic.getTopic(), PublishMessageUtils.buildContentToPublish(aim, topic));
        outboundOverQuotaEntities.put(entity, System.currentTimeMillis());
        LOGGER.info("Published new over quota outbound alarm related to entity [{}]: {}  ", entity, oq_message);
      } finally {
        RequesterContextHolder.clearContext();
      }
    }
  }

  private String getInboundAlertId(final QuotaContext qc) {
    switch (qc.getType()) {
      case GLOBAL:
        return SentiloConstants.GLOBAL_OVER_QUOTA_INBOUND_ALERT;
      default:
        return SentiloConstants.OVER_QUOTA_INBOUND_ALERT;
    }
  }

  private String getInboundMessageTemplate(final QuotaContext qc) {
    switch (qc.getType()) {
      case GLOBAL:
        return GLOBAL_INBOUND_RL_MESSAGE_TEMPLATE;
      default:
        return INBOUND_RL_MESSAGE_TEMPLATE;
    }
  }

}
