/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.test.monitor;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.utils.EventType;
import org.sentilo.platform.service.monitor.CounterContext;
import org.sentilo.platform.service.monitor.CounterServiceImpl;
import org.sentilo.platform.service.monitor.MonitorConstants;
import org.sentilo.platform.service.monitor.RequestType;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

public class CounterServiceImplTest {

  private final String entityId = "mockEntity";
  private final String tenantId = "mockTenant";

  @Mock
  private StringRedisTemplate redisTemplate;
  @Mock
  private HashOperations<String, Object, Object> hOperations;
  @Mock
  private CounterContext context;

  @InjectMocks
  private CounterServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForHash()).thenReturn(hOperations);
  }

  @Test
  public void saveDataGetCounter() {
    when(context.getRequestType()).thenReturn(RequestType.GET);
    when(context.getTenant()).thenReturn(null);
    when(context.getEntity()).thenReturn(entityId);
    when(context.getDataType()).thenReturn(EventType.DATA);
    when(context.getTotal()).thenReturn(10);

    service.save(context);

    verify(redisTemplate, times(4)).opsForHash();
    verify(hOperations).increment("counters:master", "data_get", 10);
    verify(hOperations).increment("counters:master", "requests", 10);
    verify(hOperations).increment("counters:entity:mockentity", "data_get", 10);
    verify(hOperations).increment("counters:entity:mockentity", "requests", 10);

    verify(hOperations, times(0)).increment(eq("counters:tenant:mocktenant"), any(String.class), anyInt());
    verify(hOperations, times(0)).increment(eq("counters:entity:mockentity"), eq("data_push"), anyInt());

  }

  @Test
  public void saveOrderGetCounter() {
    when(context.getRequestType()).thenReturn(RequestType.GET);
    when(context.getTenant()).thenReturn(null);
    when(context.getEntity()).thenReturn(entityId);
    when(context.getDataType()).thenReturn(EventType.ORDER);
    when(context.getTotal()).thenReturn(10);

    service.save(context);

    verify(redisTemplate, times(4)).opsForHash();
    verify(hOperations).increment("counters:master", "order_get", 10);
    verify(hOperations).increment("counters:master", "requests", 10);
    verify(hOperations).increment("counters:entity:mockentity", "order_get", 10);
    verify(hOperations).increment("counters:entity:mockentity", "requests", 10);

    verify(hOperations, times(0)).increment(eq("counters:tenant:mocktenant"), any(String.class), anyInt());
    verify(hOperations, times(0)).increment(eq("counters:entity:mockentity"), eq("data_push"), anyInt());

  }

  @Test
  public void saveAlarmGetCounter() {
    when(context.getRequestType()).thenReturn(RequestType.GET);
    when(context.getTenant()).thenReturn(null);
    when(context.getEntity()).thenReturn(entityId);
    when(context.getDataType()).thenReturn(EventType.ALARM);
    when(context.getTotal()).thenReturn(10);

    service.save(context);

    verify(redisTemplate, times(4)).opsForHash();
    verify(hOperations).increment("counters:master", "alarm_get", 10);
    verify(hOperations).increment("counters:master", "requests", 10);
    verify(hOperations).increment("counters:entity:mockentity", "alarm_get", 10);
    verify(hOperations).increment("counters:entity:mockentity", "requests", 10);

    verify(hOperations, times(0)).increment(eq("counters:tenant:mocktenant"), any(String.class), anyInt());
    verify(hOperations, times(0)).increment(eq("counters:entity:mockentity"), eq("data_push"), anyInt());

  }

  @Test
  public void saveDataPushCounter() {
    when(context.getRequestType()).thenReturn(RequestType.PUSH);
    when(context.getTenant()).thenReturn(null);
    when(context.getEntity()).thenReturn(entityId);
    when(context.getDataType()).thenReturn(EventType.DATA);
    when(context.getTotal()).thenReturn(10);

    service.save(context);

    verify(redisTemplate, times(2)).opsForHash();
    verify(hOperations).increment("counters:master", "data_push", 10);
    verify(hOperations, times(0)).increment("counters:master", "requests", 10);
    verify(hOperations).increment("counters:entity:mockentity", "data_push", 10);
    verify(hOperations, times(0)).increment("counters:entity:mockentity", "requests", 10);

    verify(hOperations, times(0)).increment(eq("counters:tenant:mocktenant"), any(String.class), anyInt());

  }

  @Test
  public void saveTenantDataGetCounter() {
    when(context.getRequestType()).thenReturn(RequestType.GET);
    when(context.getTenant()).thenReturn(tenantId);
    when(context.getEntity()).thenReturn(entityId);
    when(context.getDataType()).thenReturn(EventType.DATA);
    when(context.getTotal()).thenReturn(10);

    service.save(context);

    verify(redisTemplate, times(6)).opsForHash();
    verify(hOperations).increment("counters:master", "data_get", 10);
    verify(hOperations).increment("counters:master", "requests", 10);
    verify(hOperations).increment("counters:entity:mockentity", "data_get", 10);
    verify(hOperations).increment("counters:entity:mockentity", "requests", 10);
    verify(hOperations).increment("counters:tenant:mocktenant", "data_get", 10);
    verify(hOperations).increment("counters:tenant:mocktenant", "requests", 10);

    verify(hOperations, times(0)).increment(eq("counters:entity:mockentity"), eq("data_push"), anyInt());

  }

  @Test
  public void getInitMasterCounters() {
    when(redisTemplate.keys(MonitorConstants.TENANT_COUNTERS_KEYS_PATTERN)).thenReturn(new HashSet<String>());
    when(hOperations.entries(MonitorConstants.MASTER_COUNTERS_KEY)).thenReturn(Collections.emptyMap());

    final List<PlatformActivity> globalActivity = service.getTenantCounters();

    verify(hOperations).entries(any(String.class));
    Assert.assertTrue(globalActivity.size() == 1);
    Assert.assertTrue(globalActivity.get(0).isMaster());
    Assert.assertNull(globalActivity.get(0).getTenant());
    Assert.assertTrue(globalActivity.get(0).getTotalRequests() == 0);
    Assert.assertTrue(globalActivity.get(0).getTotalOrders() == 0);
    Assert.assertTrue(globalActivity.get(0).getTotalObs() == 0);
    Assert.assertTrue(globalActivity.get(0).getTotalAlarms() == 0);
  }

  @Test
  public void getTenantCounters() {
    final Map<Object, Object> masterHash = new HashMap<Object, Object>();
    masterHash.put("requests", "3000");
    masterHash.put("data_get", "1000");
    masterHash.put("data_put", "1800");
    masterHash.put("alarm_put", "200");

    final Set<String> countersKeys = new HashSet<String>();
    countersKeys.add("counters:tenant:mockTenant");

    when(redisTemplate.keys(MonitorConstants.TENANT_COUNTERS_KEYS_PATTERN)).thenReturn(countersKeys);
    when(hOperations.entries(MonitorConstants.MASTER_COUNTERS_KEY)).thenReturn(masterHash);

    final List<PlatformActivity> globalActivity = service.getTenantCounters();

    verify(hOperations, times(2)).entries(any(String.class));
    Assert.assertTrue(globalActivity.size() == 2);

    for (final PlatformActivity activity : globalActivity) {
      Assert.assertEquals((activity.isMaster() ? null : "mockTenant"), activity.getTenant());
      Assert.assertEquals((activity.isMaster() ? 3000l : 0l), activity.getTotalRequests());
      Assert.assertEquals(0l, activity.getTotalOrders());
      Assert.assertEquals((activity.isMaster() ? 2800l : 0l), activity.getTotalObs());
      Assert.assertEquals((activity.isMaster() ? 200l : 0l), activity.getTotalAlarms());
    }
  }

}
