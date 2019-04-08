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
package org.sentilo.agent.alert.test.trigger;

import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.common.enums.AlertTriggerType;

public class TriggerEvaluatorTest {

  private TriggerEvaluator triggerEvaluator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    triggerEvaluator = new TriggerEvaluator();
  }

  @Test
  public void passGTTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.GT);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "20");

    assertTrue("20 is not greater than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassGTTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.GT);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "28.6");

    assertTrue("28.6 is not greater than 25", result.triggerConditionChecked());
  }

  @Test
  public void passGTETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.GTE);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "20");

    assertTrue("20 is less than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassGTETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.GTE);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "25");

    assertTrue("25 is equals to 25", result.triggerConditionChecked());
  }

  @Test
  public void passLTTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.LT);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "26");

    assertTrue("26 is not less than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassLTTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.LT);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "23");

    assertTrue("23 is less than 25", result.triggerConditionChecked());
  }

  @Test
  public void passLTETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.LTE);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "26");

    assertTrue("26 is greater than 25", !result.triggerConditionChecked());
  }

  @Test
  public void noPassLTETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.LTE);
    alert.setExpression("25");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "25");

    assertTrue("25 is equals to 25", result.triggerConditionChecked());
  }

  @Test
  public void passEQTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.EQ);
    alert.setExpression("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "bca");

    assertTrue("abc is not equals to bca", !result.triggerConditionChecked());
  }

  @Test
  public void noPassEQTrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.EQ);
    alert.setExpression("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "abc");

    assertTrue("abc is equals to abc", result.triggerConditionChecked());
  }

  @Test
  public void passCHANGETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.CHANGE);

    triggerEvaluator.setPreviousValue("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "abc");

    assertTrue("Value has not changed: abc is equals to abc", !result.triggerConditionChecked());
  }

  @Test
  public void passCHANGETriggerWithNotPreviousValue() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.CHANGE);

    final TriggerResult result = triggerEvaluator.evaluate(alert, "abc");

    assertTrue("Value has not changed: abc is the first value sent", !result.triggerConditionChecked());
  }

  @Test
  public void noPassCHANGETrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.CHANGE);

    triggerEvaluator.setPreviousValue("abc");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "bca");

    assertTrue("Value has changed: bca is equals to abc", result.triggerConditionChecked());
  }

  @Test
  public void passCHANGEDELTATrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.CHANGE_DELTA);
    alert.setExpression("20");
    triggerEvaluator.setPreviousValue("5");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "4.5");

    assertTrue("4.5 don't differs from 5 more than 20%", !result.triggerConditionChecked());
  }

  @Test
  public void noPassCHANGEDELTATrigger() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.CHANGE_DELTA);
    alert.setExpression("20");
    triggerEvaluator.setPreviousValue("5");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "2");

    assertTrue("2 differs from 5 more than 20%", result.triggerConditionChecked());
  }

  @Test
  public void noNumberValue() {
    final InternalAlert alert = new InternalAlert("ALARM-1");
    alert.setTrigger(AlertTriggerType.GT);
    alert.setExpression("5");

    final TriggerResult result = triggerEvaluator.evaluate(alert, "abc");

    assertTrue("abc is not a number", result.triggerConditionChecked());
  }

}
