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
package org.sentilo.agent.alert.test.repository;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.repository.impl.FrozenRepositoryImpl;
import org.springframework.data.redis.core.Cursor;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ScanOptions;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.data.redis.core.ZSetOperations.TypedTuple;

public class FrozenRepositoryImplTest {

  private final String providerId = "providerTest";
  private final String sensorId = "sensorTest";
  private final String alertId = "alertTest";
  private final String alertId1 = "alertTest1";
  private final String alertId2 = "alertTest2";
  private final String lastSyncKey = "agent:alert:lastSync";
  private final String sortedSetKey = "alerts:frozen:sorted";

  @InjectMocks
  private FrozenRepositoryImpl repository;

  @Mock
  private RedisTemplate<String, String> redisTemplate;

  @Mock
  private ZSetOperations<String, String> operations;

  @Mock
  private ValueOperations<String, String> vOperations;

  @Mock
  private Cursor<TypedTuple<String>> scanCursor;

  @Mock
  private InternalAlert alert;
  @Mock
  private InternalAlert wrongFrozenAlert;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForZSet()).thenReturn(operations);
    when(redisTemplate.opsForValue()).thenReturn(vOperations);
  }

  @Test
  public void updateFrozenTimeouts() {
    final List<InternalAlert> alerts = Arrays.asList(alert, alert, alert);

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);
    when(alert.getExpression()).thenReturn("20");

    repository.updateFrozenTimeouts(alerts);

    verify(operations, times(alerts.size())).add(any(String.class), any(String.class), any(Double.class));
  }

  @Test
  public void updateFrozenTimeoutsWithError() {
    final List<InternalAlert> alerts = Arrays.asList(alert, alert, wrongFrozenAlert);

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);
    when(alert.getExpression()).thenReturn("20");
    when(wrongFrozenAlert.getProviderId()).thenReturn(providerId);
    when(wrongFrozenAlert.getSensorId()).thenReturn(sensorId);
    when(wrongFrozenAlert.getId()).thenReturn(alertId);
    when(wrongFrozenAlert.getExpression()).thenReturn("abc");

    repository.updateFrozenTimeouts(alerts);

    verify(operations, times(alerts.size() - 1)).add(any(String.class), any(String.class), any(Double.class));
  }

  @Test
  public void checkFrozenAlerts() {
    final Set<String> members = new TreeSet<String>(Arrays.asList("providerTest#sensorTest#alertTest1", "providerTest#sensorTest#alertTest2"));

    when(operations.rangeByScore(any(String.class), any(Double.class), any(Double.class))).thenReturn(members);

    final List<InternalAlert> alerts = repository.checkFrozenAlerts();

    verify(operations).rangeByScore(any(String.class), any(Double.class), any(Double.class));
    Assert.assertTrue(alerts.size() == 2);
    Assert.assertEquals(alerts.get(0).getProviderId(), providerId);
    Assert.assertEquals(alerts.get(0).getSensorId(), sensorId);
    Assert.assertEquals(alerts.get(0).getId(), alertId1);
    Assert.assertEquals(alerts.get(1).getProviderId(), providerId);
    Assert.assertEquals(alerts.get(1).getSensorId(), sensorId);
    Assert.assertEquals(alerts.get(1).getId(), alertId2);
  }

  @Test
  public void synchronizeAlerts() {
    final List<InternalAlert> alerts = Arrays.asList(alert, alert);

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);
    when(alert.getExpression()).thenReturn("20");
    when(alert.getUpdatedAt()).thenReturn(new Date(5l)).thenReturn(new Date(1l));
    when(vOperations.get(any(String.class))).thenReturn("2");
    when(operations.scan(eq(sortedSetKey), eq(ScanOptions.NONE))).thenReturn(scanCursor);

    repository.synchronizeAlerts(alerts);

    verify(vOperations).get(eq(lastSyncKey));
    verify(operations).add(eq(sortedSetKey), any(String.class), any(Double.class));
    verify(operations).scan(eq(sortedSetKey), eq(ScanOptions.NONE));
    verify(operations, times(0)).remove(eq(sortedSetKey), anyVararg());
    verify(vOperations).set(eq(lastSyncKey), any(String.class));
  }

  @Test
  public void synchronizeAlertsAndRemoveDeprecatedAlerts() throws IOException {
    final List<InternalAlert> alerts = Arrays.asList(alert, alert);
    final String memberKey = alert.getProviderId() + "#" + alert.getSensorId() + "#" + alert.getId();
    final String memberDeprecatedKey = alert.getProviderId() + "#" + alert.getSensorId() + "#" + alertId2;
    final DefaultTypedTuple<String> member = new DefaultTypedTuple<String>(memberKey, 1d);
    final DefaultTypedTuple<String> memberDeprecated = new DefaultTypedTuple<String>(memberDeprecatedKey, 1d);

    when(alert.getProviderId()).thenReturn(providerId);
    when(alert.getSensorId()).thenReturn(sensorId);
    when(alert.getId()).thenReturn(alertId);
    when(alert.getExpression()).thenReturn("20");
    when(alert.getUpdatedAt()).thenReturn(new Date(5l)).thenReturn(new Date(1l));
    when(vOperations.get(any(String.class))).thenReturn("2");
    when(operations.scan(eq(sortedSetKey), eq(ScanOptions.NONE))).thenReturn(scanCursor);
    when(scanCursor.hasNext()).thenReturn(true, true, true, false);
    when(scanCursor.next()).thenReturn(member).thenReturn(member).thenReturn(memberDeprecated);
    when(scanCursor.isClosed()).thenReturn(false);

    repository.synchronizeAlerts(alerts);

    verify(vOperations).get(eq(lastSyncKey));
    verify(operations).add(eq(sortedSetKey), any(String.class), any(Double.class));
    verify(operations).scan(eq(sortedSetKey), eq(ScanOptions.NONE));
    verify(operations).remove(eq(sortedSetKey), anyVararg());
    verify(vOperations).set(eq(lastSyncKey), any(String.class));
    verify(scanCursor).isClosed();
    verify(scanCursor).close();
  }
}
