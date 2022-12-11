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
package org.sentilo.agent.alert.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.listener.SensorAlertsRuleEngine;
import org.sentilo.agent.alert.listener.SensorAlertsRuleEngineFactory;
import org.sentilo.agent.alert.repository.FrozenRepository;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.service.impl.AlertServiceImpl;
import org.sentilo.common.enums.AlertTriggerType;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;

public class AlertServiceImplTest {

  @InjectMocks
  private AlertServiceImpl alertService;

  @Mock
  private MongoOperations mongoOps;
  @Mock
  private SensorAlertsRuleEngineFactory factory;
  @Mock
  private SensorAlertsRuleEngine sare_1;
  @Mock
  private SensorAlertsRuleEngine sare_2;
  @Mock
  private PublishService publishService;
  @Mock
  private FrozenRepository repository;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void init() {
    alertService.init();
    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
  }

  @Test
  public void getRuleEngine() {
    final String topicRule = "mockTopic";
    final SensorAlertsRuleEngine sare = alertService.getRuleEngine(topicRule);
    Assert.assertNull(sare);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void loadAndMonitorInternalAlerts_with_no_alerts() {
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(Collections.EMPTY_LIST);

    alertService.loadAndMonitorInternalAlerts();

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(factory, times(0)).build(anyString());
    verify(repository, times(0)).synchronizeAlerts(anyCollectionOf(InternalAlert.class));
  }

  @Test
  public void loadAndMonitorInternalAlerts() throws Exception {
    final List<InternalAlert> alerts = getInternalAlerts();
    when(factory.build("/data/prov1/sensor1")).thenReturn(sare_1);
    when(factory.build("/data/prov2/sensor2")).thenReturn(sare_2);
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(alerts);

    alertService.loadAndMonitorInternalAlerts();

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(factory).build("/data/prov1/sensor1");
    verify(factory).build("/data/prov2/sensor2");
    verify(sare_1).addAlert(alerts.get(0));
    verify(sare_2).addAlert(alerts.get(1));
    verify(repository, times(0)).synchronizeAlerts(Matchers.anyCollectionOf(InternalAlert.class));
  }

  @Test
  public void loadAndMonitorInternalFrozenAlerts() throws Exception {
    final List<InternalAlert> alerts = getFrozenAlerts();
    when(factory.build("/data/prov1/sensor1")).thenReturn(sare_1);
    when(factory.build("/data/prov2/sensor2")).thenReturn(sare_2);
    when(mongoOps.find(any(Query.class), eq(InternalAlert.class), any(String.class))).thenReturn(alerts);

    alertService.loadAndMonitorInternalAlerts();

    verify(mongoOps).find(any(Query.class), eq(InternalAlert.class), any(String.class));
    verify(factory).build("/data/prov1/sensor1");
    verify(factory).build("/data/prov2/sensor2");
    verify(sare_1).addAlert(alerts.get(0));
    verify(sare_2).addAlert(alerts.get(1));
    verify(repository).synchronizeAlerts(Matchers.anyCollectionOf(InternalAlert.class));
  }

  private List<InternalAlert> getInternalAlerts() {
    final List<InternalAlert> alerts = new ArrayList<InternalAlert>();
    final InternalAlert alert1 = new InternalAlert("1");
    alert1.setProviderId("prov1");
    alert1.setSensorId("sensor1");
    alert1.setTrigger(AlertTriggerType.GT);

    final InternalAlert alert2 = new InternalAlert("2");
    alert2.setProviderId("prov2");
    alert2.setSensorId("sensor2");
    alert2.setTrigger(AlertTriggerType.LTE);

    alerts.add(alert1);
    alerts.add(alert2);
    return alerts;
  }

  private List<InternalAlert> getFrozenAlerts() {
    final List<InternalAlert> alerts = getInternalAlerts();
    alerts.get(0).setTrigger(AlertTriggerType.FROZEN);
    alerts.get(0).setExpression("20");

    return alerts;
  }

}
