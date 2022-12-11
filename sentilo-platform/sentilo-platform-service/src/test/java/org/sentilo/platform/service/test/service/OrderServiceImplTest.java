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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.exception.ResourceOfflineException;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.ResourceOwnerContext;
import org.sentilo.platform.common.security.ResourceOwnerContextHolder;
import org.sentilo.platform.common.service.PublishService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.impl.OrderServiceImpl;

public class OrderServiceImplTest {

  @Mock
  private OrderInputMessage message;
  @Mock
  private SentiloRedisTemplate sRedisTemplate;
  @Mock
  private SentiloSequenceUtils sequenceUtils;
  @Mock
  private ResourceService resourceService;
  @Mock
  private Sensor sensor;
  @Mock
  private RequesterContext requesterContext;
  @Mock
  private ResourceOwnerContext resourceOwnerContext;
  @Mock
  private PublishService publishService;

  @InjectMocks
  private OrderServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    RequesterContextHolder.setContext(requesterContext);
    ResourceOwnerContextHolder.setContext(resourceOwnerContext);
  }

  @Test
  public void setOrder() {
    when(message.getProviderId()).thenReturn("prov1");
    when(message.getSensorId()).thenReturn("sensor1");
    when(message.getOrder()).thenReturn("stop restart");
    when(sequenceUtils.getSid(eq("prov1"), eq("sensor1"))).thenReturn(Optional.of(1l));

    service.setOrder(message);

    verify(publishService).publish(message);
  }

  @Test(expected = EventRejectedException.class)
  public void setOrderFromUnknowSensor() {
    when(message.getProviderId()).thenReturn("prov1");
    when(message.getSensorId()).thenReturn("sensor1");
    when(message.getOrder()).thenReturn("stop restart");
    doThrow(ResourceNotFoundException.class).when(resourceService).checkSensorState(eq("prov1"), eq("sensor1"));

    service.setOrder(message);

    verify(publishService, times(0)).publish(any(OrderInputMessage.class));
  }

  @Test(expected = EventRejectedException.class)
  public void setOrderFromDisabledSensor() {
    when(message.getProviderId()).thenReturn("prov1");
    when(message.getSensorId()).thenReturn("sensor1");
    when(message.getOrder()).thenReturn("stop restart");
    doThrow(ResourceOfflineException.class).when(resourceService).checkSensorState(eq("prov1"), eq("sensor1"));
    service.setOrder(message);

    verify(publishService, times(0)).publish(any(OrderInputMessage.class));
  }

  @Test
  public void getLastOrdersFromProviderAndSensor() {
    final String providerId = "prov1";
    final String sensorId = "sensor1";
    final Set<String> soids = buildSoids();

    when(message.getSensorId()).thenReturn(sensorId);
    when(message.getProviderId()).thenReturn(providerId);
    when(resourceService.getSensorsToInspect(providerId, sensorId)).thenReturn(buildSids());
    when(resourceService.getSensor(anyLong())).thenReturn(Optional.of(sensor));
    when(sensor.getProvider()).thenReturn(providerId);
    when(sensor.getSensor()).thenReturn(sensorId);
    when(sequenceUtils.getSid(notNull(String.class), notNull(String.class))).thenReturn(Optional.of(1l));
    when(sRedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSoids());

    service.getLastOrders(message);

    verify(message).getSensorId();
    verify(message, times(2)).getProviderId();
    verify(sRedisTemplate, times(2)).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(sRedisTemplate, times(2 * soids.size())).hGetAll(anyString());
  }

  @Test
  public void getLastObservationsFromProvider() {
    final String provider = "prov1";
    final Set<String> sdids = buildSoids();

    when(message.getProviderId()).thenReturn(provider);
    when(sequenceUtils.getPid(notNull(String.class))).thenReturn(Optional.of(1l));
    when(resourceService.getSensorsToInspect(provider, null)).thenReturn(buildSids());
    when(resourceService.getSensor(anyLong())).thenReturn(Optional.of(sensor));
    when(sRedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(buildSoids());

    service.getLastOrders(message);

    verify(message).getSensorId();
    verify(message, times(2)).getProviderId();
    verify(sRedisTemplate, times(1 * sdids.size())).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(sRedisTemplate, times(2 * sdids.size())).hGetAll(anyString());
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
