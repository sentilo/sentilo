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
package org.sentilo.agent.alert.test.trigger;

import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.Date;

import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.agent.alert.utils.enums.AlarmTriggerType;
import org.springframework.util.ReflectionUtils;

public class TriggerEvaluatorTest {

  private TriggerEvaluator triggerEvaluator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    triggerEvaluator = new TriggerEvaluator();
  }

  @Test
  public void passGTTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.GT);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "20");

    assertTrue("20 is not greater than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassGTTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.GT);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "28.6");

    assertTrue("28.6 is not greater than 25", result.triggerConditionChecked());
  }

  @Test
  public void passGTETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.GT);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "20");

    assertTrue("20 is less than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassGTETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.GTE);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "25");

    assertTrue("25 is equals to 25", result.triggerConditionChecked());
  }

  @Test
  public void passLTTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.LT);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "26");

    assertTrue("26 is not less than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassLTTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.LT);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "23");

    assertTrue("23 is less than 25", result.triggerConditionChecked());
  }

  @Test
  public void passLTETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.LTE);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "26");

    assertTrue("26 is greater than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassLTETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.LTE);
    alarm.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "25");

    assertTrue("25 is equals to 25", result.triggerConditionChecked());
  }

  @Test
  public void passEQTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.EQ);
    alarm.setExpression("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "bca");

    assertTrue("abc is not equals to bca", !result.triggerConditionChecked());
  }

  @Test
  public void noPassEQTrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.EQ);
    alarm.setExpression("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "abc");

    assertTrue("abc is equals to abc", result.triggerConditionChecked());
  }

  @Test
  public void passCHANGETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.CHANGE);

    triggerEvaluator.setLastAcceptedValue("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "abc");

    assertTrue("Value has not changed: abc is equals to abc", !result.triggerConditionChecked());
  }

  @Test
  public void passCHANGETriggerWithNotPreviousValue() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.CHANGE);

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "abc");

    assertTrue("Value has not changed: abc is the first value sent", !result.triggerConditionChecked());
  }

  @Test
  public void noPassCHANGETrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.CHANGE);

    triggerEvaluator.setLastAcceptedValue("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "bca");

    assertTrue("Value has changed: bca is equals to abc", result.triggerConditionChecked());
  }

  @Test
  public void passCHANGEDELTATrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.CHANGE_DELTA);
    alarm.setExpression("20");
    triggerEvaluator.setLastAcceptedValue("5");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "4.5");

    assertTrue("4.5 don't differs from 5 more than 20%", !result.triggerConditionChecked());
  }

  @Test
  public void noPassCHANGEDELTATrigger() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.CHANGE_DELTA);
    alarm.setExpression("20");
    triggerEvaluator.setLastAcceptedValue("5");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "2");

    assertTrue("2 differs from 5 more than 20%", result.triggerConditionChecked());
  }

  @Test
  public void noNumberValue() {
    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.GT);
    alarm.setExpression("5");

    final TriggerResult result = triggerEvaluator.evaluate(alarm, "abc");

    assertTrue("abc is not a number", result.triggerConditionChecked());
  }

  @Test
  public void passFrozenTrigger() throws Exception {
    final Field field = TriggerEvaluator.class.getDeclaredField("tsLastAcceptedValue");
    ReflectionUtils.makeAccessible(field);

    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.FROZEN);
    alarm.setExpression("20");
    field.set(triggerEvaluator, new Date());

    final TriggerResult result = triggerEvaluator.checkFrozen(alarm);

    assertTrue("Sensor has not been frozen more than 20 minutes", !result.triggerConditionChecked());
  }

  @Test
  public void noPassFrozenTrigger() throws Exception {
    final Field field = TriggerEvaluator.class.getDeclaredField("tsLastAcceptedValue");
    ReflectionUtils.makeAccessible(field);

    final Alarm alarm = new Alarm("ALARM-1");
    alarm.setTrigger(AlarmTriggerType.FROZEN);
    alarm.setExpression("20");
    final Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MINUTE, -30);
    field.set(triggerEvaluator, calendar.getTime());

    final TriggerResult result = triggerEvaluator.checkFrozen(alarm);

    assertTrue("Sensor has been frozen more than 20 minutes", result.triggerConditionChecked());
  }
}
