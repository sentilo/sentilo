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
package org.sentilo.agent.alert.test.domain;


import java.util.Date;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.scheduler.CheckFrozenAlarmJob;
import org.sentilo.agent.alert.utils.enums.AlarmTriggerType;
import org.sentilo.agent.alert.utils.enums.AlarmType;


public class AlarmTest {

  final String id = "alert1";

  @Test
  public void testEquals() {
    Assert.assertEquals(new Alarm(id), new Alarm(id));
    Assert.assertNotEquals(new Alarm(id), new CheckFrozenAlarmJob());
  }

  @Test
  public void testHashCode() {
    Assert.assertTrue(new Alarm(id).hashCode() == new Alarm(id).hashCode());
  }

  @Test
  public void testAttributes() {
    Alarm alarm = new Alarm();

    final Date createdAt = new Date();
    final Date updateAt = new Date();
    final String name = "alarm23";
    final String description = "Alarm 23 description";
    final AlarmType type = AlarmType.EXTERNAL;
    final AlarmTriggerType trigger = AlarmTriggerType.CHANGE_DELTA;
    final String expression = "15%";
    final String providerId = "provider1";
    final String componentId = "component1";
    final String sensorId = "sensor1";

    alarm.setId(id);
    alarm.setCreatedAt(createdAt);
    alarm.setUpdateAt(updateAt);
    alarm.setName(name);
    alarm.setDescription(description);
    alarm.setType(type);
    alarm.setTrigger(trigger);
    alarm.setExpression(expression);
    alarm.setProviderId(providerId);
    alarm.setComponentId(componentId);
    alarm.setSensorId(sensorId);

    Assert.assertEquals(id, alarm.getId());
    Assert.assertEquals(createdAt, alarm.getCreatedAt());
    Assert.assertEquals(updateAt, alarm.getUpdateAt());
    Assert.assertEquals(name, alarm.getName());
    Assert.assertEquals(description, alarm.getDescription());
    Assert.assertEquals(type, alarm.getType());
    Assert.assertEquals(trigger, alarm.getTrigger());
    Assert.assertEquals(expression, alarm.getExpression());
    Assert.assertEquals(providerId, alarm.getProviderId());
    Assert.assertEquals(componentId, alarm.getComponentId());
    Assert.assertEquals(sensorId, alarm.getSensorId());
  }

}
