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
package org.sentilo.common.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.exception.MessageNotWritableException;

public class StringMessageConverterTest {

  static final String DATA_TOPIC = "/data/provider23";
  static final String ORDER_TOPIC = "/order/provider23";
  static final String ALARM_TOPIC = "/alarm/alert1";
  static final String MESSAGE = "stop start sensors";
  static final String ALARM_MESSAGE = "Temperature is greater than 34";
  static final String INFO = "21/03/2013T14:25:39#@#stop start sensors";
  static final String INVALID_INFO = "stop start sensors";

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Test
  public void marshallDataEventMessage() throws MessageNotWritableException {
    final EventMessage notification = buildMockDataEventMessage();
    final String json = converter.marshal(notification);
    final String body = "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\",\"type\":\"data\"}";
    assertNotNull(json);
    assertEquals(json, body);
  }

  @Test
  public void marshallOrderEventMessage() throws MessageNotWritableException {
    final EventMessage notification = buildMockOrderEventMessage();
    final String json = converter.marshal(notification);
    final String body =
        "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/order/provider23\",\"type\":\"order\"}";
    assertNotNull(json);
    assertEquals(json, body);
  }

  @Test
  public void marshallAlarmEventMessage() throws MessageNotWritableException {
    final EventMessage notification = buildMockAlarmEventMessage();
    final String json = converter.marshal(notification);
    final String body =
        "{\"message\":\"Temperature is greater than 34\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/alarm/alert1\",\"type\":\"alarm\"}";
    assertNotNull(json);
    assertEquals(json, body);
  }

  @Test
  public void unmarshallBody() throws MessageNotWritableException {
    final String body =
        "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\", \"sender\":\"/data/provider23\"}";
    final EventMessage notification = (EventMessage) converter.unmarshal(body, EventMessage.class);
    assertNotNull(notification);
    assertEquals(MESSAGE, notification.getMessage());
    assertEquals(DATA_TOPIC, notification.getTopic());
  }

  @Test
  public void unmarshallNullBody() throws MessageNotWritableException {
    final EventMessage notification = (EventMessage) converter.unmarshal(null, EventMessage.class);
    assertNotNull(notification);
    assertNull(notification.getMessage());
    assertNull(notification.getTopic());
  }

  private EventMessage buildMockDataEventMessage() {
    final EventMessage event = new EventMessage();
    event.setTopic(DATA_TOPIC);
    event.setType(EventType.DATA.name().toLowerCase());
    event.setTimestamp("21/03/2013T14:25:39");
    event.setMessage(MESSAGE);
    return event;
  }

  private EventMessage buildMockOrderEventMessage() {
    final EventMessage event = new EventMessage();
    event.setTopic(ORDER_TOPIC);
    event.setType(EventType.ORDER.name().toLowerCase());
    event.setTimestamp("21/03/2013T14:25:39");
    event.setMessage(MESSAGE);
    return event;
  }

  private EventMessage buildMockAlarmEventMessage() {
    final EventMessage event = new EventMessage();
    event.setTopic(ALARM_TOPIC);
    event.setType(EventType.ALARM.name().toLowerCase());
    event.setTimestamp("21/03/2013T14:25:39");
    event.setMessage(ALARM_MESSAGE);
    return event;
  }

}
