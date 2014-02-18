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
package org.sentilo.platform.client.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.sentilo.common.domain.SubscribeType;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.platform.client.core.domain.Endpoint;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.sentilo.platform.client.core.parser.SubscribeMessageConverter;

public class SubscribeMessageConverterTest {

  static final String PROVIDER_ID = "provider1";
  static final String SENSOR_ID = "sensor1";
  static final Endpoint endpoint = new Endpoint("http://dev.connecta.cat");

  SubscribeMessageConverter converter = new SubscribeMessageConverter();

  @Test
  public void buildSubscribeBody() throws MessageNotWritableException {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, endpoint);
    final String json = converter.marshall(message);
    final String body = "{\"endpoint\":\"http://dev.connecta.cat\"}";
    assertNotNull(json);
    assertEquals(body, json);
  }
}
