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
package org.sentilo.agent.common.test.service;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.common.service.impl.AsyncPendingEventServiceImpl;
import org.sentilo.agent.common.utils.Utils;
import org.sentilo.common.domain.EventMessage;

public class AsyncPendingEventServiceImplTest {

  private final String mockDataPatternTopic = "/data/provider1/*";
  private final String mockAlarmTopic = "/alarm/mockAlert1";

  @Mock
  private AbstractMessageListenerImpl messageListener;

  @Mock
  private AbstractMessageListenerImpl messageListener2;

  @Mock
  private EventMessage eventMessage;

  @InjectMocks
  private AsyncPendingEventServiceImpl pendingEventService;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    // Initialize topicMapping map
    pendingEventService.addMessageListener(messageListener, Utils.buildTopic(mockDataPatternTopic));
    pendingEventService.addMessageListener(messageListener, Utils.buildTopic(mockAlarmTopic));
  }

  @Test
  public void process() {
    when(eventMessage.getTopic()).thenReturn("/data/provider1/sensor23", "/data/provider2/sensor2", "/alarm/mockAlert1");

    pendingEventService.process(eventMessage);
    pendingEventService.process(eventMessage);
    pendingEventService.process(eventMessage);

    verify(messageListener, times(2)).doWithMessage(eventMessage);
  }

  @Test
  public void addNewListenerAndProcess() {
    when(eventMessage.getTopic()).thenReturn("/data/provider1/sensor23", "/data/provider2/sensor2", "/alarm/mockAlert1");

    pendingEventService.addMessageListener(messageListener2, Utils.buildTopic("/data/provider2/sensor2"));
    pendingEventService.process(eventMessage);
    pendingEventService.process(eventMessage);
    pendingEventService.process(eventMessage);

    verify(messageListener, times(2)).doWithMessage(eventMessage);
    verify(messageListener2).doWithMessage(eventMessage);
  }
}
