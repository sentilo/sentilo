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
package org.sentilo.platform.service.test.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyDouble;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.enums.SensorState;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.ResourceOwnerContext;
import org.sentilo.platform.common.security.ResourceOwnerContextHolder;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.impl.DataServiceImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.springframework.data.redis.listener.Topic;
import org.springframework.test.util.ReflectionTestUtils;

import com.google.common.collect.ImmutableMap;

public class DataServiceImplTest {

  @InjectMocks
  private DataServiceImpl service;
  @Mock
  private DataInputMessage inputMessage;
  @Mock
  private SentiloRedisTemplate sRedisTemplate;
  @Mock
  private SentiloSequenceUtils sequenceUtils;
  @Mock
  private ResourceService resourceService;
  @Mock
  private RequesterContext requesterContext;
  @Mock
  private ResourceOwnerContext resourceOwnerContext;
  @Mock
  private QueryFilterParams queryFilterParams;
  @Mock
  private InternalAlarmService internalAlarmService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    RequesterContextHolder.setContext(requesterContext);
    ResourceOwnerContextHolder.setContext(resourceOwnerContext);
  }

  @After
  public void tearDown() {
    RequesterContextHolder.clearContext();
    ResourceOwnerContextHolder.clearContext();
  }

  @Test
  public void setObservations() {
    final String provider = "prov1";
    final String sensor1 = "sensor1";
    final List<Observation> observations = buildObservations(provider, sensor1);
    when(inputMessage.getObservations()).thenReturn(observations);
    // when(sequenceUtils.getSid(eq(provider), eq(sensor1))).thenReturn(new Long(1));
    when(resourceService.getSensor(eq(provider), eq(sensor1))).thenReturn(new Sensor(1L, provider, sensor1, SensorState.online.name(), "2"));
    when(sequenceUtils.getSdid()).thenReturn(new Long(10));

    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensor1);

    service.setObservations(inputMessage);

    // Para cada observacion se pasara una vez por los metodos indicados
    verify(inputMessage).getObservations();
    verify(resourceService, times(observations.size())).getSensor(eq(provider), eq(sensor1));
    verify(sequenceUtils, times(0)).getSid(provider, sensor1);
    verify(sequenceUtils, times(observations.size())).getSdid();
    verify(sRedisTemplate, times(observations.size())).hmSet(eq("sdid:10"), anyMapOf(String.class, String.class));
    verify(sRedisTemplate, times(observations.size())).zAdd(eq("sid:1:observations"), anyDouble(), eq("10"));
    verify(sRedisTemplate, times(observations.size())).publish(eq(topic.getTopic()), anyString());
  }

  @Test
  public void setObservationsFromGhostSensor() {
    final Long sid = 1L;
    final String provider = "prov1";
    final String sensorId = "sensor1";
    final Sensor sensor = new Sensor(provider, sensorId);

    final List<Observation> observations = buildObservations(provider, sensorId);
    ReflectionTestUtils.setField(service, "rejectUnknownSensors", false);
    when(inputMessage.getObservations()).thenReturn(observations);
    when(resourceService.getSensor(eq(provider), eq(sensorId))).then(new Answer<Sensor>() {

      @Override
      public Sensor answer(final InvocationOnMock invocation) throws Throwable {
        // Clean ghost parameters from previous calls to emulate new sensor instance
        sensor.setState(null);
        sensor.setSid(null);
        return sensor;
      }

    });
    when(sequenceUtils.getSid(eq(provider), eq(sensorId))).thenReturn(sid);
    when(sequenceUtils.getSdid()).thenReturn(new Long(10));
    when(resourceService.registerGhostSensorIfNeedBe(sensor)).then(new Answer<Long>() {

      @Override
      public Long answer(final InvocationOnMock invocation) throws Throwable {
        final Long sid = sequenceUtils.getSid(sensor.getProvider(), sensor.getSensor());
        sensor.setState(SensorState.ghost);
        sensor.setSid(sid);
        return sid;
      }

    });

    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensorId);

    service.setObservations(inputMessage);

    verify(inputMessage).getObservations();
    verify(sequenceUtils, times(observations.size())).getSid(provider, sensorId);
    verify(sequenceUtils, times(observations.size())).getSdid();
    verify(sRedisTemplate, times(observations.size())).hmSet(eq("sdid:10"), anyMapOf(String.class, String.class));
    verify(sRedisTemplate, times(observations.size())).zAdd(eq("sid:1:observations"), anyDouble(), eq("10"));
    verify(sRedisTemplate, times(observations.size())).publish(eq(topic.getTopic()), anyString());
    verify(internalAlarmService, times(observations.size())).publishGhostSensorAlarm(any());
  }

  @Test
  public void setObservationsFromUnknownSensors() {
    boolean dataRejected = false;
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensor);
    final List<Observation> observations = buildObservations(provider, sensor);
    when(inputMessage.getObservations()).thenReturn(observations);
    when(resourceService.getSensor(eq(provider), eq(sensor))).thenReturn(new Sensor(provider, sensor));

    try {
      service.setObservations(inputMessage);
    } catch (final EventRejectedException ere) {
      dataRejected = true;
    }

    Assert.assertTrue(dataRejected);
    verify(inputMessage).getObservations();
    verify(sequenceUtils, times(0)).getSid(provider, sensor);
    verify(sequenceUtils, times(0)).getSdid();
    verify(sRedisTemplate, times(0)).publish(eq(topic.getTopic()), anyString());
  }

  @Test
  public void setObservationsFromDisabledSensors() {
    boolean dataRejected = false;
    final Long sid = 1L;
    final String provider = "prov1";
    final String sensorId = "sensor1";
    final Sensor sensor = new Sensor(sid, provider, sensorId, SensorState.offline.name(), "10");
    final List<Observation> observations = buildObservations(provider, sensorId);

    when(inputMessage.getObservations()).thenReturn(observations);
    when(sequenceUtils.getSid(eq(provider), eq(sensorId))).thenReturn(sid);
    when(resourceService.getSensor(eq(provider), eq(sensorId))).thenReturn(sensor);

    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensorId);

    try {
      service.setObservations(inputMessage);
    } catch (final EventRejectedException ere) {
      dataRejected = true;
    }

    Assert.assertTrue(dataRejected);
    verify(inputMessage).getObservations();
    verify(sequenceUtils, times(0)).getSid(provider, sensorId);
    verify(sequenceUtils, times(0)).getSdid();
    verify(sRedisTemplate, times(0)).publish(eq(topic.getTopic()), anyString());
  }

  @Test
  public void setObservationsFromPartiallyUnknownSensors() {
    boolean dataRejected = false;
    final String provider = "prov1";
    final String sensor1 = "sensor1";
    final String sensor2 = "sensor2";
    final List<Observation> observations = buildObservations(provider, sensor1, sensor2);
    when(inputMessage.getObservations()).thenReturn(observations);
    when(sequenceUtils.getSid(eq(provider), eq(sensor1))).thenReturn(null);
    when(sequenceUtils.getSid(eq(provider), eq(sensor2))).thenReturn(2L);
    when(resourceService.getSensor(eq(provider), eq(sensor1))).thenReturn(new Sensor(provider, sensor1));
    when(resourceService.getSensor(eq(provider), eq(sensor2))).thenReturn(new Sensor(2L, provider, sensor1, SensorState.online.name(), "2"));

    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensor2);

    try {
      service.setObservations(inputMessage);
    } catch (final EventRejectedException ere) {
      dataRejected = true;
    }

    Assert.assertTrue(dataRejected);
    verify(inputMessage).getObservations();
    verify(sequenceUtils, times(0)).getSid(provider, sensor1);
    verify(sequenceUtils, times(observations.size() / 2)).getSdid();
    verify(sRedisTemplate, times(observations.size() / 2)).publish(eq(topic.getTopic()), anyString());
  }

  @Test
  public void deleteLastObservationsFromProviderAndSensorWithoutObservations() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(sRedisTemplate.zRange(eq("sid:1:observations"), anyLong(), anyLong())).thenReturn(null);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage, times(2)).getSensorId();
    verify(inputMessage).getProviderId();
    verify(sRedisTemplate).zRange(eq("sid:1:observations"), anyLong(), anyLong());
    verify(sRedisTemplate, times(0)).zRemRangeByRank(eq("sid:1:observations"), anyLong(), anyLong());
    verify(sRedisTemplate, times(0)).del(anyString());
  }

  @Test
  public void deleteLastObservationsFromProviderAndSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids = buildSdids();

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(sRedisTemplate.zRange(eq("sid:1:observations"), anyLong(), anyLong())).thenReturn(sdids);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage, times(2)).getSensorId();
    verify(inputMessage).getProviderId();
    verify(sRedisTemplate).zRange(eq("sid:1:observations"), anyLong(), anyLong());
    verify(sRedisTemplate, times(1)).zRemRangeByRank(eq("sid:1:observations"), anyLong(), anyLong());
    verify(sRedisTemplate, times(1)).del(anyString());
  }

  @Test
  public void deleteLastObservationsFromProviderWithoutSensors() {
    final String provider = "prov1";

    when(inputMessage.getSensorId()).thenReturn(null);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(sequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsFromProvider(provider)).thenReturn(null);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage).getProviderId();
    verify(resourceService).getSensorsFromProvider(provider);
    verify(sRedisTemplate, times(0)).zRange(anyString(), anyLong(), anyLong());
    verify(sRedisTemplate, times(0)).zRemRangeByRank(anyString(), anyLong(), anyLong());
    verify(sRedisTemplate, times(0)).del(anyString());
  }

  @Test
  public void deleteLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sids = buildSids();

    when(inputMessage.getSensorId()).thenReturn(null);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(sequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsFromProvider(provider)).thenReturn(buildSids());
    when(sRedisTemplate.zRange(anyString(), anyLong(), anyLong())).thenReturn(buildSdids());

    service.deleteLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage).getProviderId();
    verify(resourceService).getSensorsFromProvider(provider);
    verify(sRedisTemplate, times(1 * sids.size())).zRange(anyString(), anyLong(), anyLong());
    verify(sRedisTemplate, times(1 * sids.size())).zRemRangeByRank(anyString(), anyLong(), anyLong());
    verify(sRedisTemplate, times(1 * sids.size())).del(anyString());

  }

  @Test
  public void getLastObservationsFromProviderAndSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids = new HashSet<String>(Arrays.asList("121", "122"));
    final Set<String> sids = new HashSet<String>(Arrays.asList("1"));
    final int limit = 2;

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(inputMessage.hasQueryFilters()).thenReturn(true);
    when(inputMessage.getQueryFilters()).thenReturn(queryFilterParams);
    when(queryFilterParams.getLimit()).thenReturn(limit);
    when(resourceService.getSensorsToInspect(provider, sensor)).thenReturn(sids);
    when(sequenceUtils.getSid(provider, sensor)).thenReturn(new Long(1));
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1))).thenReturn(sdids);
    when(sRedisTemplate.hGetAll("sdid:121")).thenReturn(ImmutableMap.of("data", "23", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(sRedisTemplate.hGetAll("sdid:122")).thenReturn(ImmutableMap.of("data", "24", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(resourceService.getSensor(1l)).thenReturn(new Sensor(provider, sensor));

    service.getLastObservations(inputMessage);

    verify(inputMessage, times(limit)).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1));
    verify(sRedisTemplate, times(limit)).hGetAll(anyString());
  }

  @Test
  public void getLastObservationsFromProviderAndSensorWithExpiredData() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids_1 = new HashSet<String>(Arrays.asList("121", "122", "123"));
    final Set<String> sdids_2 = new HashSet<String>(Arrays.asList("123", "124", "125"));
    final Set<String> sids = new HashSet<String>(Arrays.asList("1"));
    final int limit = 2;

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(inputMessage.hasQueryFilters()).thenReturn(true);
    when(inputMessage.getQueryFilters()).thenReturn(queryFilterParams);
    when(queryFilterParams.getLimit()).thenReturn(limit);
    when(resourceService.getSensorsToInspect(provider, sensor)).thenReturn(sids);
    when(sequenceUtils.getSid(provider, sensor)).thenReturn(new Long(1));
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1))).thenReturn(sdids_1);
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(limit), eq(limit + 1))).thenReturn(sdids_2);
    when(sRedisTemplate.hGetAll("sdid:122")).thenReturn(ImmutableMap.of("data", "23", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(sRedisTemplate.hGetAll("sdid:124")).thenReturn(ImmutableMap.of("data", "24", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(resourceService.getSensor(1l)).thenReturn(new Sensor(provider, sensor));

    service.getLastObservations(inputMessage);

    verify(inputMessage, times(limit)).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1));
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(limit), eq(limit + 1));
    verify(sRedisTemplate, times(4)).hGetAll(anyString());
  }

  @Test
  public void getLastObservationsFromProviderAndSensorWithExpiredDataAndReturningLess() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids_1 = new HashSet<String>(Arrays.asList("121", "122", "123", "124"));
    final Set<String> sdids_2 = new HashSet<String>(Arrays.asList("124", "125", "126", "127"));
    final Set<String> sdids_3 = new HashSet<String>(Arrays.asList("127"));
    final Set<String> sids = new HashSet<String>(Arrays.asList("1"));
    final int limit = 3;

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(inputMessage.hasQueryFilters()).thenReturn(true);
    when(inputMessage.getQueryFilters()).thenReturn(queryFilterParams);
    when(queryFilterParams.getLimit()).thenReturn(limit);
    when(resourceService.getSensorsToInspect(provider, sensor)).thenReturn(sids);
    when(sequenceUtils.getSid(provider, sensor)).thenReturn(new Long(1));
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1))).thenReturn(sdids_1);
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(limit), eq(limit + 1))).thenReturn(sdids_2);
    when(sRedisTemplate.zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(2 * limit), eq(limit + 1))).thenReturn(sdids_3);
    when(sRedisTemplate.hGetAll("sdid:126")).thenReturn(ImmutableMap.of("data", "23", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(sRedisTemplate.hGetAll("sdid:127")).thenReturn(ImmutableMap.of("data", "24", "sid", "1", "ts", Long.toString(System.currentTimeMillis())));
    when(resourceService.getSensor(1l)).thenReturn(new Sensor(provider, sensor));

    service.getLastObservations(inputMessage);

    verify(inputMessage, times(2)).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(0), eq(limit + 1));
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(limit), eq(limit + 1));
    verify(sRedisTemplate).zRevRangeByScore(eq("sid:1:observations"), anyDouble(), anyDouble(), eq(2 * limit), eq(limit + 1));
    verify(sRedisTemplate, times(7)).hGetAll(anyString());
  }

  // @Test
  public void getLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sdids = buildSdids();

    when(inputMessage.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsToInspect(provider, null)).thenReturn(buildSids());
    when(sRedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSdids());

    service.getLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(resourceService).getSensorsToInspect(provider, null);
    verify(sRedisTemplate, times(1 * sdids.size())).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(sRedisTemplate, times(2 * sdids.size())).hGetAll(anyString());
  }

  private List<Observation> buildObservations(final String provider, final String... sensors) {
    final List<Observation> observations = new ArrayList<Observation>();
    for (final String sensor : sensors) {
      final Observation obs1 = new Observation(provider, sensor, "12", System.currentTimeMillis());
      final Observation obs2 = new Observation(provider, sensor, "14", System.currentTimeMillis() + 6000);

      observations.add(obs1);
      observations.add(obs2);
    }

    return observations;
  }

  private Set<String> buildSdids() {
    final Set<String> observations = new HashSet<String>();
    observations.add("1");
    observations.add("2");
    return observations;
  }

  private Set<String> buildSids() {
    final Set<String> sensors = new HashSet<String>();
    sensors.add("1");
    sensors.add("2");
    return sensors;
  }
}
