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
package org.sentilo.platform.service.test.monitor;

import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.service.monitor.MetricServiceImpl;
import org.sentilo.platform.service.monitor.MonitorConstants;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

public class MetricServiceImplTest {

  @Mock
  private StringRedisTemplate redisTemplate;
  @Mock
  private HashOperations<String, Object, Object> hOperations;

  @InjectMocks
  private MetricServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    when(redisTemplate.opsForHash()).thenReturn(hOperations);
  }

  @Test
  public void buildAvgHashKey() {
    final String avgMasterKey = (String) ReflectionTestUtils.invokeMethod(service, "buildAvgHashKey", "counters:master");
    final String avgEntityKey = (String) ReflectionTestUtils.invokeMethod(service, "buildAvgHashKey", "counters:entity:mockEntity");

    Assert.assertEquals("avgs:master", avgMasterKey);
    Assert.assertEquals("avgs:entity:mockEntity", avgEntityKey);
  }

  @Test
  public void computeInstantAvg() {
    final Float instantAvg1 = (Float) ReflectionTestUtils.invokeMethod(service, "computeInstantAvg", 1000l, 0l, 30l);
    final Float instantAvg2 = (Float) ReflectionTestUtils.invokeMethod(service, "computeInstantAvg", 8167l, 2000l, 30l);

    Assert.assertEquals(0f, instantAvg1, 0);
    Assert.assertEquals(Float.valueOf("205.57"), instantAvg2, 0);
  }

  @Test
  public void computeInitialPerformance() {
    final String countersKey = "counters:master";
    final String avgsKey = "avgs:master";
    final Map<Object, Object> avgsHash = new HashMap<Object, Object>();
    final String totalRequests = "10000";
    final long currentTs = System.currentTimeMillis();
    when(hOperations.get(countersKey, MonitorConstants.TOTAL_REQUESTS_FIELD)).thenReturn(totalRequests);
    when(hOperations.entries(avgsKey)).thenReturn(avgsHash);

    ReflectionTestUtils.invokeMethod(service, "computePerformance", countersKey, currentTs);

    Assert.assertFalse(CollectionUtils.isEmpty(avgsHash));
    Assert.assertEquals(totalRequests, avgsHash.get(MonitorConstants.TOTAL_REQUESTS_FIELD));
    Assert.assertEquals("0.0", avgsHash.get(MonitorConstants.INSTANT_AVG_FIELD));

  }

  @Test
  public void computePerformance() {

    final String countersKey = "counters:master";
    final String avgsKey = "avgs:master";
    final long currentTs = System.currentTimeMillis();
    final Map<Object, Object> avgsHash = new HashMap<Object, Object>();
    avgsHash.put(MonitorConstants.MAX_AVG_FIELD, "1900.45");
    avgsHash.put(MonitorConstants.MAX_DAILY_AVG_FIELD, "276.32");
    avgsHash.put(MonitorConstants.TIMESTAMP_FIELD, Long.toString(currentTs - 60 * 1000));
    avgsHash.put(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD, Long.toString(currentTs - 3 * 24 * 60 * 60 * 1000));
    avgsHash.put(MonitorConstants.TOTAL_REQUESTS_FIELD, "100000");

    final String currentTotalRequests = "540076";

    when(hOperations.get(countersKey, MonitorConstants.TOTAL_REQUESTS_FIELD)).thenReturn(currentTotalRequests);
    when(hOperations.entries(avgsKey)).thenReturn(avgsHash);

    ReflectionTestUtils.invokeMethod(service, "computePerformance", countersKey, currentTs);

    Assert.assertFalse(CollectionUtils.isEmpty(avgsHash));
    Assert.assertEquals(currentTotalRequests, avgsHash.get(MonitorConstants.TOTAL_REQUESTS_FIELD));
    Assert.assertEquals("7334.6", avgsHash.get(MonitorConstants.INSTANT_AVG_FIELD));
    Assert.assertEquals("7334.6", avgsHash.get(MonitorConstants.MAX_DAILY_AVG_FIELD));
    Assert.assertEquals("7334.6", avgsHash.get(MonitorConstants.MAX_AVG_FIELD));
    Assert.assertEquals(Long.toString(currentTs), avgsHash.get(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD));
  }

  @Test
  public void computePerformance2() {

    final String countersKey = "counters:master";
    final String avgsKey = "avgs:master";
    final long currentTs = System.currentTimeMillis();
    final Map<Object, Object> avgsHash = new HashMap<Object, Object>();
    avgsHash.put(MonitorConstants.MAX_AVG_FIELD, "1900.45");
    avgsHash.put(MonitorConstants.MAX_DAILY_AVG_FIELD, "276.32");
    avgsHash.put(MonitorConstants.TIMESTAMP_FIELD, Long.toString(currentTs - 60 * 1000));
    avgsHash.put(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD, Long.toString(currentTs - 3 * 24 * 60 * 60 * 1000));
    avgsHash.put(MonitorConstants.TOTAL_REQUESTS_FIELD, "100000");

    final String currentTotalRequests = "114117";

    when(hOperations.get(countersKey, MonitorConstants.TOTAL_REQUESTS_FIELD)).thenReturn(currentTotalRequests);
    when(hOperations.entries(avgsKey)).thenReturn(avgsHash);

    ReflectionTestUtils.invokeMethod(service, "computePerformance", countersKey, currentTs);

    Assert.assertFalse(CollectionUtils.isEmpty(avgsHash));
    Assert.assertEquals(currentTotalRequests, avgsHash.get(MonitorConstants.TOTAL_REQUESTS_FIELD));
    Assert.assertEquals("235.28", avgsHash.get(MonitorConstants.INSTANT_AVG_FIELD));
    Assert.assertEquals("276.32", avgsHash.get(MonitorConstants.MAX_DAILY_AVG_FIELD));
    Assert.assertEquals("1900.45", avgsHash.get(MonitorConstants.MAX_AVG_FIELD));
    Assert.assertEquals(Long.toString(currentTs - 3 * 24 * 60 * 60 * 1000), avgsHash.get(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD));
  }

  @Test
  public void initialComputePerformance() {

    final String countersKey = "counters:master";
    final String avgsKey = "avgs:master";
    final long currentTs = System.currentTimeMillis();
    final Map<Object, Object> avgsHash = new HashMap<Object, Object>();

    final String currentTotalRequests = null;

    when(hOperations.get(countersKey, MonitorConstants.TOTAL_REQUESTS_FIELD)).thenReturn(currentTotalRequests);
    when(hOperations.entries(avgsKey)).thenReturn(avgsHash);

    ReflectionTestUtils.invokeMethod(service, "computePerformance", countersKey, currentTs);

    Assert.assertFalse(CollectionUtils.isEmpty(avgsHash));
    Assert.assertEquals("0", avgsHash.get(MonitorConstants.TOTAL_REQUESTS_FIELD));
    Assert.assertEquals("0.0", avgsHash.get(MonitorConstants.INSTANT_AVG_FIELD));
    Assert.assertEquals("0.0", avgsHash.get(MonitorConstants.MAX_DAILY_AVG_FIELD));
    Assert.assertEquals(null, avgsHash.get(MonitorConstants.MAX_AVG_FIELD));
    Assert.assertEquals(null, avgsHash.get(MonitorConstants.TIMESTAMP_MAX_AVG_FIELD));
  }

}
