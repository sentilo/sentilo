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
package org.sentilo.agent.activity.monitor.test.service;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.activity.monitor.repository.ActivityMonitorRepository;
import org.sentilo.agent.activity.monitor.service.CatalogService;
import org.sentilo.agent.activity.monitor.service.impl.ActivityMonitorServiceImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.SentiloConstants;

public class ActivityMonitorServiceImplTest {

  @InjectMocks
  private ActivityMonitorServiceImpl service;
  @Mock
  private CatalogService catalogService;
  @Mock
  private EventMessage eventMessage;
  @Mock
  private ActivityMonitorRepository repository;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void processDataEvent() {
    when(eventMessage.getType()).thenReturn(EventType.DATA.name());

    service.process(eventMessage);

    verify(repository).publishMessageToElasticSearch(eventMessage);
  }

  @Test
  public void processOrderEvent() {
    when(eventMessage.getType()).thenReturn(EventType.ORDER.name());

    service.process(eventMessage);

    verify(repository).publishMessageToElasticSearch(eventMessage);
  }

  @Test
  public void processAlarmEvent() {
    final String alertId = "1";
    final String sensor = "mockSensor";
    final String provider = "mockProvider";
    when(eventMessage.getType()).thenReturn(EventType.ALARM.name());
    when(eventMessage.getAlert()).thenReturn(alertId);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getProvider()).thenReturn(provider);

    service.process(eventMessage);

    verify(catalogService).getAdditionalFields(alertId);
    verify(catalogService).getSensorType(provider, sensor);
    verify(repository).publishMessageToElasticSearch(eventMessage);
  }

  @Test
  public void processExternalAlarmEvent() {
    final String alertId = "1";

    when(eventMessage.getType()).thenReturn(EventType.ALARM.name());
    when(eventMessage.getAlert()).thenReturn(alertId);

    service.process(eventMessage);

    verify(catalogService, times(0)).getAdditionalFields(alertId);
    verify(catalogService, times(0)).getSensorType(anyString(), anyString());
    verify(repository).publishMessageToElasticSearch(eventMessage);
  }

  @Test
  public void processGhostSensorAlarmEvent() {
    final String alertId = SentiloConstants.GHOST_SENSOR_ALERT;
    final String sensor = "mockSensor";
    final String provider = "mockProvider";
    when(eventMessage.getType()).thenReturn(EventType.ALARM.name());
    when(eventMessage.getAlert()).thenReturn(alertId);
    when(eventMessage.getSensor()).thenReturn(sensor);
    when(eventMessage.getProvider()).thenReturn(provider);

    service.process(eventMessage);

    verify(catalogService, times(0)).getAdditionalFields(alertId);
    verify(catalogService, times(0)).getSensorType(provider, sensor);
    verify(repository).publishMessageToElasticSearch(eventMessage);
  }

  @Test
  public void filterByTenant() {
    final EventMessage event1 = Mockito.mock(EventMessage.class);
    final EventMessage event2 = Mockito.mock(EventMessage.class);
    final EventMessage event3 = Mockito.mock(EventMessage.class);

    final String tenantFilter = "MockTenant1,MockTenant2";
    System.setProperty("sentilo.tenant.filter", tenantFilter);

    when(event1.getType()).thenReturn(EventType.DATA.name());
    when(event2.getType()).thenReturn(EventType.DATA.name());
    when(event3.getType()).thenReturn(EventType.DATA.name());
    when(event1.getTenant()).thenReturn("wrongTenant");
    when(event2.getTenant()).thenReturn("MockTenant1");
    when(event3.getTenant()).thenReturn("MockTenant2");

    service.init();

    // First event should be rejected
    service.process(event1);
    // Second event should be processed
    service.process(event2);
    // Third event should also be processed
    service.process(event3);

    verify(repository, times(0)).publishMessageToElasticSearch(event1);
    verify(repository).publishMessageToElasticSearch(event2);
    verify(repository).publishMessageToElasticSearch(event3);
  }

}
