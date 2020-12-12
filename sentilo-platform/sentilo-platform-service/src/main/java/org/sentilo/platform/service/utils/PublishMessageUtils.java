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
package org.sentilo.platform.service.utils;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.ResourceOwnerContextHolder;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.StringUtils;

public abstract class PublishMessageUtils {

  private static final StringMessageConverter converter = new DefaultStringMessageConverter();

  private PublishMessageUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String buildContentToPublish(final AlarmInputMessage message, final Topic topic) {
    final Long timestamp = System.currentTimeMillis();

    final EventMessage event = new EventMessage();
    event.setAlert(message.getAlertId());
    event.setAlertType(message.getAlertType());
    event.setProvider(message.getProviderId());
    event.setSensor(message.getSensorId());
    event.setMessage(message.getMessage());
    event.setTimestamp(DateUtils.timestampToString(timestamp));
    event.setTime(timestamp);
    event.setType(EventType.ALARM.name());
    event.setTopic(topic.getTopic());
    event.setPublisher(message.getSender());

    setCustomsFields(event);

    return converter.marshal(event);
  }

  public static String buildContentToPublish(final OrderInputMessage message, final Topic topic) {
    final Long timestamp = System.currentTimeMillis();

    final EventMessage event = new EventMessage();
    event.setProvider(message.getProviderId());
    event.setSensor(message.getSensorId());
    event.setMessage(message.getOrder());
    event.setTimestamp(DateUtils.timestampToString(timestamp));
    event.setTime(timestamp);
    event.setType(EventType.ORDER.name());
    event.setTopic(topic.getTopic());

    setCustomsFields(event);

    return converter.marshal(event);
  }

  public static String buildContentToPublish(final Observation message, final Topic topic) {
    final EventMessage event = new EventMessage();
    event.setProvider(message.getProvider());
    event.setSensor(message.getSensor());
    event.setMessage(message.getValue());
    event.setTimestamp(DateUtils.timestampToString(message.getTimestamp()));
    event.setTime(message.getTimestamp());
    event.setLocation(message.getLocation());
    event.setType(EventType.DATA.name());
    event.setTopic(topic.getTopic());

    setCustomsFields(event);

    return converter.marshal(event);
  }

  private static void setCustomsFields(final EventMessage event) {
    if (RequesterContextHolder.hasContext()) {
      if (!StringUtils.hasText(event.getPublisher())) {
        event.setPublisher(RequesterContextHolder.getContext().getEntityId());
      }

      event.setPublishedAt(RequesterContextHolder.getContext().getRequestTimestamp());
      event.setPublisherTenant(RequesterContextHolder.getContext().getTenantId());
    }

    if (ResourceOwnerContextHolder.hasContext()) {
      event.setTenant(ResourceOwnerContextHolder.getContext().getTenantId());
    }
  }
}
