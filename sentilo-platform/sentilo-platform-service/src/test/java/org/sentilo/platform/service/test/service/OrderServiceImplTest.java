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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;
import org.sentilo.platform.service.impl.OrderServiceImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;

public class OrderServiceImplTest {

  @Mock
  private OrderInputMessage message;
  @Mock
  private JedisTemplate<String, String> jedisTemplate;
  @Mock
  private JedisSequenceUtils jedisSequenceUtils;
  @Mock
  private ResourceService resourceService;
  @Mock
  private Sensor sensor;
  @InjectMocks
  private OrderServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void setOrder() {
    when(message.getProviderId()).thenReturn("prov1");
    when(message.getSensorId()).thenReturn("sensor1");
    when(message.getOrder()).thenReturn("stop restart");

    final String channel = ChannelUtils.buildTopic(PubSubChannelPrefix.order, message.getProviderId(), message.getSensorId()).getTopic();
    service.setOrder(message);

    verify(resourceService).registerProviderIfNecessary(message.getProviderId());
    verify(resourceService).registerSensorIfNecessary(message.getSensorId(), message.getProviderId());
    verify(jedisTemplate).publish(eq(channel), anyString());
  }

  @Test
  public void getLastOrdersFromProviderAndSensor() {
    final String providerId = "prov1";
    final String sensorId = "sensor1";
    final Set<String> soids = buildSoids();

    when(message.getSensorId()).thenReturn(sensorId);
    when(message.getProviderId()).thenReturn(providerId);
    when(resourceService.getSensorsToInspect(providerId, sensorId)).thenReturn(buildSids());
    when(resourceService.getSensor(anyLong())).thenReturn(sensor);
    when(sensor.getProvider()).thenReturn(providerId);
    when(sensor.getSensor()).thenReturn(sensorId);
    when(jedisSequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSoids());

    service.getLastOrders(message);

    verify(message).getSensorId();
    verify(message, times(2)).getProviderId();
    verify(jedisTemplate, times(2)).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(2 * soids.size())).hGetAll(anyString());
  }

  @Test
  public void getLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sdids = buildSoids();

    when(message.getProviderId()).thenReturn(provider);
    when(jedisSequenceUtils.getPid(notNull(String.class))).thenReturn(new Long(1));
    when(resourceService.getSensorsToInspect(provider, null)).thenReturn(buildSids());
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSoids());

    service.getLastOrders(message);

    verify(message).getSensorId();
    verify(message, times(2)).getProviderId();
    verify(jedisTemplate, times(1 * sdids.size())).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(2 * sdids.size())).hGetAll(anyString());
  }

  private Set<String> buildSoids() {
    final Set<String> orders = new HashSet<String>();
    orders.add("1");
    orders.add("2");
    return orders;
  }

  private Set<String> buildSids() {
    final Set<String> sensors = new HashSet<String>();
    sensors.add("1");
    sensors.add("2");
    return sensors;
  }

}
