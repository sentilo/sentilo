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
package org.sentilo.web.catalog.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.ApplyAlertRuleResponse;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.repository.AlertRuleRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.impl.AlertRuleServiceImpl;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.util.CollectionUtils;

public class AlertRuleServiceImplTest extends AbstractBaseTest {

  private final String provider = "mockProvider";
  private final String componentType = "mockComponentType";
  private final String sensorType = "mockSensorType";

  @InjectMocks
  private AlertRuleServiceImpl alertRuleService;

  @Mock
  private AlertRuleRepository repository;

  @Mock
  private ComponentService componentService;

  @Mock
  private SensorService sensorService;

  @Mock
  private AlertService alertService;

  @Mock
  private MongoOperations mongoOperations;

  @Mock
  private SearchFilterResult<Sensor> sensorSearchFilterResult;

  @Mock
  private SearchFilterResult<Component> componentSearchFilterResult;

  @Mock
  private SearchFilterResult<Alert> alertSearchFilterResult;

  @Mock
  private AlertRule alertRule;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void emptyFindSensors() throws Exception {
    final List<Sensor> emptySensorsList = alertRuleService.findSensors(null, null, null);

    Assert.assertTrue(CollectionUtils.isEmpty(emptySensorsList));
  }

  @Test
  public void findSensorsFilteredBySensorType() throws Exception {
    final List<Sensor> filteredSensorsList = generateRandomList(Sensor.class);

    when(sensorService.search(any(SearchFilter.class))).thenReturn(sensorSearchFilterResult);
    when(sensorSearchFilterResult.getContent()).thenReturn(filteredSensorsList);

    final List<Sensor> sensorsList = alertRuleService.findSensors(provider, null, sensorType);

    Assert.assertEquals(filteredSensorsList.size(), sensorsList.size());
    verify(sensorService).search(any(SearchFilter.class));
  }

  @Test
  public void findSensorsFilteredByComponentType() throws Exception {
    final List<Sensor> filteredSensorsList = generateRandomList(Sensor.class);
    final List<Component> filteredComponentsList = generateRandomList(Component.class);

    when(componentService.search(any(SearchFilter.class))).thenReturn(componentSearchFilterResult);
    when(componentSearchFilterResult.getContent()).thenReturn(filteredComponentsList);
    when(sensorService.search(any(SearchFilter.class))).thenReturn(sensorSearchFilterResult);
    when(sensorSearchFilterResult.getContent()).thenReturn(filteredSensorsList);

    final List<Sensor> sensorsList = alertRuleService.findSensors(provider, componentType, sensorType);

    Assert.assertEquals(filteredSensorsList.size() * filteredComponentsList.size(), sensorsList.size());
    verify(componentService).search(any(SearchFilter.class));
    verify(sensorService, times(filteredComponentsList.size())).search(any(SearchFilter.class));
  }

  @Test
  public void createAlerts() throws Exception {
    final List<Sensor> filteredSensorsList = generateRandomList(Sensor.class);

    when(sensorService.search(any(SearchFilter.class))).thenReturn(sensorSearchFilterResult);
    when(sensorSearchFilterResult.getContent()).thenReturn(filteredSensorsList);

    when(alertRule.getProviderId()).thenReturn(provider);
    when(alertRule.getSensorType()).thenReturn(sensorType);
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.FROZEN);

    when(alertService.search(any(SearchFilter.class))).thenReturn(alertSearchFilterResult);

    final ApplyAlertRuleResponse response = alertRuleService.createAlerts(alertRule);

    Assert.assertTrue(filteredSensorsList.size() == response.getTotalSensors());
    Assert.assertTrue(filteredSensorsList.size() == response.getGeneratedAlerts());
  }

  @Test
  public void discardExistingAlerts() throws Exception {
    final List<Sensor> filteredSensorsList = generateRandomList(Sensor.class);

    when(sensorService.search(any(SearchFilter.class))).thenReturn(sensorSearchFilterResult);
    when(sensorSearchFilterResult.getContent()).thenReturn(filteredSensorsList);

    when(alertRule.getProviderId()).thenReturn(provider);
    when(alertRule.getSensorType()).thenReturn(sensorType);
    when(alertRule.getTrigger()).thenReturn(AlertTriggerType.FROZEN);

    when(alertService.search(any(SearchFilter.class))).thenReturn(alertSearchFilterResult);
    when(alertSearchFilterResult.hasContent()).thenReturn(Boolean.TRUE);

    final ApplyAlertRuleResponse response = alertRuleService.createAlerts(alertRule);

    Assert.assertTrue(filteredSensorsList.size() == response.getTotalSensors());
    Assert.assertTrue(0 == response.getGeneratedAlerts());
  }

}
