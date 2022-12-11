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
package org.sentilo.agent.location.test.listener;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.location.batch.AsyncComponentLocationUpdater;
import org.sentilo.agent.location.listener.MessageListenerImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;

public class MessageListenerImplTest {

  final String provider = "provider1";
  final String sensor = "sensor1";
  final String location = "2.145874 43.215478";
  final String timestamp = "11/03/2021T10:25:39";
  final long time = 123456789l;

  @Mock
  private AsyncComponentLocationUpdater asyncResourceUpdater;

  @InjectMocks
  private final MessageListenerImpl listener = new MessageListenerImpl("TEST");

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void doWithMessage() {
    final EventMessage eventMessage = buildMockDataEventMessage(true);
    listener.doWithMessage(eventMessage);

    verify(asyncResourceUpdater).add(eventMessage);
  }

  @Test
  public void doWithMessage_when_locationNoFilledIn() {
    final EventMessage eventMessage = buildMockDataEventMessage(false);

    listener.doWithMessage(eventMessage);

    verify(asyncResourceUpdater, times(0)).add(any(EventMessage.class));
  }

  private EventMessage buildMockDataEventMessage(final boolean withLocation) {
    final EventMessage event = new EventMessage();
    event.setTopic("/data/" + provider + "/" + sensor);
    event.setProvider(provider);
    event.setSensor(sensor);
    event.setType(EventType.DATA.name().toLowerCase());
    event.setTimestamp(timestamp);
    event.setMessage("21");
    event.setLocation(withLocation ? location : "");
    event.setTime(time);
    return event;
  }

}
