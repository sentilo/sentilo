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
package org.sentilo.agent.relational.domain;

import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.domain.EventMessage;
import org.springframework.util.StringUtils;

/**
 * Representa un missatge que es rep per una subscripció
 */
public class EndpointMessage {

  private String message;
  private String timestamp;
  private String topic;
  private final EventMessage event;

  public EndpointMessage(final EventMessage eventMessage) {
    super();
    timestamp = eventMessage.getTimestamp();
    message = eventMessage.getMessage();
    topic = eventMessage.getTopic();
    event = eventMessage;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(final String message) {
    this.message = message;
  }

  public String getTopic() {
    return topic;
  }

  public void setTopic(final String topic) {
    this.topic = topic;
  }

  public void setTimestamp(final String timestamp) {
    this.timestamp = timestamp;
  }

  public String getTimestamp() {
    return timestamp;
  }

  /**
   * Retorna l'observació continguda en el missatge rebut
   *
   * @return l'observació
   */
  public Observation getObservation() {
    final Observation obs = new Observation();
    obs.setSourceEvent(event);

    // El topic siempre tiene el formato /data/provider/sensor
    final String[] ids = topic.split(Constants.TOPIC_TOKEN);

    obs.setProvider(ids[2]);
    if (ids.length == 4) {
      obs.setSensor(ids[3]);
    }

    obs.setValue(message);
    obs.setTimestamp(timestamp);
    obs.setEventTimestamp(event.getTime());
    obs.setPublishedAt(event.getPublishedAt());
    obs.setPublisher(event.getPublisher());

    if (StringUtils.hasText(event.getLocation())) {
      obs.setLocation(event.getLocation());
    }

    return obs;
  }

  /**
   * Retorna l'alarma continguda en el missatge rebut
   *
   * @return l'alarma
   */
  public Alarm getAlarm() {
    final Alarm alarm = new Alarm();
    alarm.setSourceEvent(event);

    final String[] ids = topic.split(Constants.TOPIC_TOKEN);
    alarm.setAlarm(ids[2]);
    alarm.setMessage(message);
    alarm.setTimestamp(timestamp);
    alarm.setEventTimestamp(event.getTime());
    alarm.setPublishedAt(event.getPublishedAt());
    alarm.setPublisher(event.getPublisher());
    return alarm;
  }

  /**
   * Retorna l'ordre continguda en el missatge rebut
   *
   * @return l'ordre
   */
  public Order getOrder() {
    final Order order = new Order();
    order.setSourceEvent(event);

    final String[] ids = topic.split(Constants.TOPIC_TOKEN);
    order.setProvider(ids[2]);
    if (ids.length == 4) {
      order.setSensor(ids[3]);
    }

    order.setMessage(message);
    order.setTimestamp(timestamp);
    order.setEventTimestamp(event.getTime());
    order.setPublishedAt(event.getPublishedAt());
    order.setPublisher(event.getPublisher());
    return order;
  }

  @Override
  public String toString() {
    return "EndpointMessage [message=" + message + ", topic=" + topic + ", timestamp=" + timestamp + "]";
  }

}
