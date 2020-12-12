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
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.agent.location.batch.AsyncCatalogResourceUpdater;
import org.sentilo.agent.location.listener.MessageListenerImpl;
import org.sentilo.common.cache.LRUCache;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.common.utils.SentiloConstants;

public class MessageListenerImplTest {

  final String provider = "provider1";
  final String sensor = "sensor1";
  final String location = "21.3456 41.3456";
  final String timestamp = "02/10/2014T09:15:23";
  final long time = 123456789l;

  @Mock
  private LRUCache<String, String> sensorLocationsCache;
  @Mock
  private AsyncCatalogResourceUpdater asyncResourceUpdater;
  @Mock
  private AgentMetricsCounter metricsCounters;

  @Mock
  private EventMessage eventMessage;
  @InjectMocks
  private MessageListenerImpl listener = new MessageListenerImpl("TEST");

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void doWithMessage() {
    when(eventMessage.getProvider()).thenReturn(provider);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getLocation()).thenReturn(location);
    when(eventMessage.getTimestamp()).thenReturn(timestamp);
    when(eventMessage.getTime()).thenReturn(time);
    when(sensorLocationsCache.get(any(String.class))).thenReturn(null);

    listener.doWithMessage(eventMessage);

    verify(asyncResourceUpdater).addResourceToUpdate(any(SensorLocationElement.class));
  }

  @Test
  public void locationNoFilledIn() {
    when(eventMessage.getProvider()).thenReturn(provider);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getTimestamp()).thenReturn(timestamp);
    when(eventMessage.getTime()).thenReturn(time);
    when(eventMessage.getLocation()).thenReturn(null);

    listener.doWithMessage(eventMessage);

    verify(asyncResourceUpdater, times(0)).addResourceToUpdate(any(SensorLocationElement.class));
  }

  @Test
  public void locationNoChanged() {
    when(eventMessage.getProvider()).thenReturn(provider);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getLocation()).thenReturn(location);
    when(eventMessage.getTimestamp()).thenReturn(timestamp);
    when(eventMessage.getTime()).thenReturn(time);
    when(sensorLocationsCache.get(provider + SentiloConstants.SENTILO_INTERNAL_TOKEN + sensor + SentiloConstants.SENTILO_INTERNAL_TOKEN + time))
        .thenReturn(location);

    listener.doWithMessage(eventMessage);

    verify(asyncResourceUpdater, times(0)).addResourceToUpdate(any(SensorLocationElement.class));
  }

}
