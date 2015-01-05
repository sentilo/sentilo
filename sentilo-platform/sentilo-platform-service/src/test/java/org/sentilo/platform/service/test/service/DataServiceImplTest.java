/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;
import org.sentilo.platform.service.impl.DataServiceImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.springframework.data.redis.listener.Topic;

public class DataServiceImplTest {

  @InjectMocks
  private DataServiceImpl service;
  @Mock
  private DataInputMessage inputMessage;
  @Mock
  private JedisTemplate<String, String> jedisTemplate;
  @Mock
  private JedisSequenceUtils jedisSequenceUtils;
  @Mock
  private ResourceService resourceService;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void setObservations() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final List<Observation> observations = buildObservations();
    when(inputMessage.getObservations()).thenReturn(observations);
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(jedisSequenceUtils.getSdid()).thenReturn(new Long(10));

    final Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, provider, sensor);

    service.setObservations(inputMessage);

    // Para cada observacion se pasara una vez por los metodos indicados
    verify(inputMessage).getObservations();
    // verify(jedisSequenceUtils, times(1*observations.size())).getPid(provider);
    verify(jedisSequenceUtils, times(observations.size())).getSid(provider, sensor);
    verify(jedisSequenceUtils, times(observations.size())).getSdid();
    verify(jedisTemplate, times(observations.size())).hmSet(eq("sdid:10"), anyMapOf(String.class, String.class));
    verify(jedisTemplate, times(observations.size())).zAdd(eq("sid:1:observations"), anyDouble(), eq("10"));
    verify(jedisTemplate, times(observations.size())).publish(eq(topic.getTopic()), anyString());
  }

  @Test
  public void deleteLastObservationsFromProviderAndSensorWithoutObservations() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRange(eq("sid:1:observations"), anyLong(), anyLong())).thenReturn(null);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage, times(2)).getSensorId();
    verify(inputMessage).getProviderId();
    verify(jedisTemplate).zRange(eq("sid:1:observations"), anyLong(), anyLong());
    verify(jedisTemplate, times(0)).zRemRangeByRank(eq("sid:1:observations"), anyLong(), anyLong());
    verify(jedisTemplate, times(0)).del(anyString());

  }

  @Test
  public void deleteLastObservationsFromProviderAndSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids = buildSdids();

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRange(eq("sid:1:observations"), anyLong(), anyLong())).thenReturn(sdids);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage, times(2)).getSensorId();
    verify(inputMessage).getProviderId();
    verify(jedisTemplate).zRange(eq("sid:1:observations"), anyLong(), anyLong());
    verify(jedisTemplate, times(1)).zRemRangeByRank(eq("sid:1:observations"), anyLong(), anyLong());
    verify(jedisTemplate, times(1)).del(anyString());
  }

  @Test
  public void deleteLastObservationsFromProviderWithoutSensors() {
    final String provider = "prov1";

    when(inputMessage.getSensorId()).thenReturn(null);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsFromProvider(provider)).thenReturn(null);

    service.deleteLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage).getProviderId();
    verify(resourceService).getSensorsFromProvider(provider);
    verify(jedisTemplate, times(0)).zRange(anyString(), anyLong(), anyLong());
    verify(jedisTemplate, times(0)).zRemRangeByRank(anyString(), anyLong(), anyLong());
    verify(jedisTemplate, times(0)).del(anyString());
  }

  @Test
  public void deleteLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sids = buildSids();

    when(inputMessage.getSensorId()).thenReturn(null);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsFromProvider(provider)).thenReturn(buildSids());
    when(jedisTemplate.zRange(anyString(), anyLong(), anyLong())).thenReturn(buildSdids());

    service.deleteLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage).getProviderId();
    verify(resourceService).getSensorsFromProvider(provider);
    verify(jedisTemplate, times(1 * sids.size())).zRange(anyString(), anyLong(), anyLong());
    verify(jedisTemplate, times(1 * sids.size())).zRemRangeByRank(anyString(), anyLong(), anyLong());
    verify(jedisTemplate, times(1 * sids.size())).del(anyString());

  }

  @Test
  public void getLastObservationsFromProviderAndSensor() {
    final String provider = "prov1";
    final String sensor = "sensor1";
    final Set<String> sdids = buildSdids();

    when(inputMessage.getSensorId()).thenReturn(sensor);
    when(inputMessage.getProviderId()).thenReturn(provider);
    when(resourceService.getSensorsToInspect(provider, sensor)).thenReturn(buildSids());
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSdids());

    service.getLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(jedisTemplate, times(2)).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(2 * sdids.size())).hGetAll(anyString());
  }

  @Test
  public void getLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sdids = buildSdids();

    when(inputMessage.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsToInspect(provider, null)).thenReturn(buildSids());
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSdids());

    service.getLastObservations(inputMessage);

    verify(inputMessage).getSensorId();
    verify(inputMessage, times(2)).getProviderId();
    verify(resourceService).getSensorsToInspect(provider, null);
    verify(jedisTemplate, times(1 * sdids.size())).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(2 * sdids.size())).hGetAll(anyString());
  }

  private List<Observation> buildObservations() {
    final List<Observation> observations = new ArrayList<Observation>();
    final Observation obs1 = new Observation("prov1", "sensor1", "1", System.currentTimeMillis());
    final Observation obs2 = new Observation("prov1", "sensor1", "1", System.currentTimeMillis() + 6000);
    observations.add(obs1);
    observations.add(obs2);

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
