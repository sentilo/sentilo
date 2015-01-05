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
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.notNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;
import org.sentilo.platform.service.impl.AlarmServiceImpl;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;

public class AlarmServiceImplTest {

  @Mock
  private AlarmInputMessage message;
  @Mock
  private JedisTemplate<String, String> jedisTemplate;
  @Mock
  private JedisSequenceUtils jedisSequenceUtils;
  @Mock
  private ResourceService resourceService;
  @InjectMocks
  private AlarmServiceImpl service;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void setAlarm() {
    when(message.getMessage()).thenReturn("Evento de alarma");
    when(message.getAlertId()).thenReturn("alarm1");

    final String channel = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlertId()).getTopic();
    service.setAlarm(message);

    verify(jedisTemplate).publish(eq(channel), anyString());
  }

  @Test
  public void getLastMessagesFromAlarm() {
    final String alarm = "alarm1";
    final Set<String> amids = buildAmids();

    when(message.getAlertId()).thenReturn(alarm);
    when(jedisSequenceUtils.getAid(notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(amids);

    service.getLastMessages(message);

    verify(message, times(2)).getAlertId();
    verify(jedisTemplate).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(amids.size())).hGetAll(anyString());
  }

  @Test
  public void getEmptyLastMessagesFromAlarm() {
    final String alarm = "alarm1";

    when(message.getAlertId()).thenReturn(alarm);
    when(jedisSequenceUtils.getAid(notNull(String.class))).thenReturn(new Long(1));
    when(jedisTemplate.zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt())).thenReturn(Collections.<String>emptySet());

    service.getLastMessages(message);

    verify(message, times(1)).getAlertId();
    verify(jedisTemplate).zRevRangeByScore(anyString(), anyDouble(), anyDouble(), anyInt(), anyInt());
    verify(jedisTemplate, times(0)).hGetAll(anyString());
  }

  private Set<String> buildAmids() {
    final Set<String> alarms = new HashSet<String>();
    alarms.add("1");
    alarms.add("2");
    return alarms;
  }
}
