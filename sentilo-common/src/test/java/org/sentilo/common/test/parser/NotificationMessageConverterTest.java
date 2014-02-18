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
package org.sentilo.common.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.sentilo.common.domain.NotificationMessage;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.common.parser.NotificationMessageConverter;

public class NotificationMessageConverterTest {

  static final String TOPIC = "/data/provider23";
  static final String MESSAGE = "stop start sensors";
  static final String INFO = "21/03/2013T14:25:39#@#stop start sensors";
  static final String INVALID_INFO = "stop start sensors";

  NotificationMessageConverter converter = new NotificationMessageConverter();

  @Test
  public void marshallBody() throws MessageNotWritableException {
    final NotificationMessage notification = new NotificationMessage(INFO, TOPIC);
    final String json = converter.marshall(notification);
    final String body = "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\"}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test(expected = ArrayIndexOutOfBoundsException.class)
  public void marshallInvalidMessage() {
    final NotificationMessage notification = new NotificationMessage(INVALID_INFO, TOPIC);
    converter.marshall(notification);
  }

  @Test
  public void unmarshallBody() throws MessageNotWritableException {
    final String body = "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\"}";
    final NotificationMessage notification = converter.unmarshall(body);
    assertNotNull(notification);
    assertEquals(MESSAGE, notification.getMessage());
    assertEquals(TOPIC, notification.getTopic());
  }

  @Test
  public void unmarshallNullBody() throws MessageNotWritableException {
    final NotificationMessage notification = converter.unmarshall(null);
    assertNotNull(notification);
    assertNull(notification.getMessage());
    assertNull(notification.getTopic());
  }
}
