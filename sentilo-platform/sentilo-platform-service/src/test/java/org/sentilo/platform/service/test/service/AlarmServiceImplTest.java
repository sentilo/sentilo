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

import static org.mockito.Matchers.anyDouble;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogAlertResponseMessage;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.domain.Alert;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.exception.ResourceNotFoundException;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.common.security.ResourceOwnerContext;
import org.sentilo.platform.common.security.ResourceOwnerContextHolder;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.SentiloRedisTemplate;
import org.sentilo.platform.service.dao.SentiloSequenceUtils;
import org.sentilo.platform.service.impl.AlarmServiceImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;

public class AlarmServiceImplTest {

  final static String ALERT_ID = "alert1";

  @Mock
  private AlarmInputMessage message;
  @Mock
  private SentiloRedisTemplate sRedisTemplate;
  @Mock
  private SentiloSequenceUtils sequenceUtils;
  @Mock
  private ResourceService resourceService;
  @Mock
  private CatalogService catalogService;
  @Mock
  private CatalogAlertResponseMessage catalogResponseMessage;
  @Mock
  private Alert alert;
  @Mock
  private RequesterContext requesterContext;
  @Mock
  private ResourceOwnerContext resourceOwnerContext;

  @InjectMocks
  private AlarmServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    RequesterContextHolder.setContext(requesterContext);
    ResourceOwnerContextHolder.setContext(resourceOwnerContext);
  }

  @Test
  public void setAlarm() {
    when(message.getMessage()).thenReturn("Alarm event");
    when(message.getAlertId()).thenReturn(ALERT_ID);
    when(resourceService.existsAlert(ALERT_ID)).thenReturn(Boolean.TRUE);
    when(resourceService.isAlertDisabled(ALERT_ID)).thenReturn(Boolean.FALSE);
    final String channel = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlertId()).getTopic();

    service.setAlarm(message);

    verify(sRedisTemplate).publish(eq(channel), anyString());
  }

  @Test(expected = EventRejectedException.class)
  public void setAlarmFromDisabledAlert() {
    when(message.getMessage()).thenReturn("Alarm event");
    when(message.getAlertId()).thenReturn(ALERT_ID);
    when(resourceService.existsAlert(ALERT_ID)).thenReturn(Boolean.TRUE);
    when(resourceService.isAlertDisabled(ALERT_ID)).thenReturn(Boolean.TRUE);

    service.setAlarm(message);
  }

  @Test(expected = EventRejectedException.class)
  public void setAlarmFromUnknowAlert() {
    when(message.getMessage()).thenReturn("Alarm event");
    when(message.getAlertId()).thenReturn(ALERT_ID);
    when(resourceService.existsAlert(ALERT_ID)).thenReturn(Boolean.FALSE);

    service.setAlarm(message);
  }

  @Test
  public void getLastAlertAlarms() {
    final Set<String> amids = buildAmids();

    when(message.getAlertId()).thenReturn(ALERT_ID);
    when(sequenceUtils.getAid(notNull(String.class))).thenReturn(new Long(1));
    when(sRedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(amids);

    service.getLastAlarms(message);

    verify(message, times(2)).getAlertId();
    verify(sRedisTemplate).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(sRedisTemplate, times(amids.size())).hGetAll(anyString());
  }

  @Test
  public void getEmptyLastAlertAlarms() {
    when(message.getAlertId()).thenReturn(ALERT_ID);
    when(sequenceUtils.getAid(notNull(String.class))).thenReturn(new Long(1));
    when(sRedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(Collections.<String>emptySet());

    service.getLastAlarms(message);

    verify(message, times(1)).getAlertId();
    verify(sRedisTemplate).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(sRedisTemplate, times(0)).hGetAll(anyString());
  }

  @Test(expected = ResourceNotFoundException.class)
  public void getUnknownAlertOwner() {
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(1L);
    when(resourceService.getAlert(1L)).thenReturn(null);

    service.getAlertOwner(ALERT_ID);
  }

  @Test
  public void getAlertOwner() {
    final String owner = "mockOwner";
    when(sequenceUtils.getAid(ALERT_ID)).thenReturn(1L);
    when(resourceService.getAlert(1L)).thenReturn(alert);
    when(alert.getEntity()).thenReturn(owner);

    final String actualOwner = service.getAlertOwner(ALERT_ID);

    Assert.assertEquals(owner, actualOwner);
  }

  private Set<String> buildAmids() {
    final Set<String> alarms = new HashSet<String>();
    alarms.add("1");
    alarms.add("2");
    return alarms;
  }
}
