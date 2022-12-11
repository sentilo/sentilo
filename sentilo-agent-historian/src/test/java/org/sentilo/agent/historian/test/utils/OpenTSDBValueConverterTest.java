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
package org.sentilo.agent.historian.test.utils;

import static org.junit.Assert.fail;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.historian.domain.OpenTSDBDataPoint;
import org.sentilo.agent.historian.utils.OpenTSDBDataPointMarshaller;
import org.sentilo.agent.historian.utils.OpenTSDBValueConverter;
import org.sentilo.common.domain.EventMessage;

public class OpenTSDBValueConverterTest {

  @Test
  public void testGetSimpleValue() {

    try {
      Assert.assertEquals(new Float(3.00), OpenTSDBValueConverter.getSimpleValue("3.00"));
      Assert.assertEquals(new Long(5), OpenTSDBValueConverter.getSimpleValue("5"));
      Assert.assertEquals(new Integer(0), OpenTSDBValueConverter.getSimpleValue("false"));
      Assert.assertEquals(new Integer(1), OpenTSDBValueConverter.getSimpleValue("true"));
    } catch (final ParseException pe) {
      Assert.fail("Should not fail on numeric or boolean value");
    }

    try {
      OpenTSDBValueConverter.getSimpleValue("I am a nasty string");
      Assert.fail("Should have thrown an Exception on a string value");
    } catch (final ParseException pe) {
    }

    try {
      OpenTSDBValueConverter.getSimpleValue("1.1E99");
      Assert.assertEquals(null, OpenTSDBValueConverter.getSimpleValue("-1.1E99"));
      Assert.fail("Should have failed on outer bound value");
    } catch (final ParseException pe) {
    }

    try {
      Assert.assertEquals(null, OpenTSDBValueConverter.getSimpleValue("-1.1E99"));
      Assert.fail("Should have failed on outer bound value");
    } catch (final ParseException pe) {
    }
  }

  @Test
  public void testIsComplexValue() {
    final String jsonValue = "{\"summary\":{\"avg\":67.7788,\"max\":68.3408,\"min\":67.0837,\"samples\":76,\"duration\":900}}";
    Assert.assertTrue(OpenTSDBValueConverter.isComplexValue(jsonValue));
    Assert.assertFalse(OpenTSDBValueConverter.isComplexValue("34.0"));
    Assert.assertFalse(OpenTSDBValueConverter.isComplexValue(""));
  }

  @Test
  public void testExtractMeasuresFromComplexType() {
    final String measureName = "data.opentrends.test";
    final String jsonValue = "{\"summary\":{\"avg\":67.7788,\"max\":68.3408,\"min\":67.0837,\"samples\":76,\"duration\":900}}";

    final Map<String, Object> expected = new HashMap<String, Object>();
    expected.put(measureName + ".summary.avg", new Float(67.7788));
    expected.put(measureName + ".summary.max", new Float(68.3408));
    expected.put(measureName + ".summary.min", new Float(67.0837));
    expected.put(measureName + ".summary.samples", new Long(76));
    expected.put(measureName + ".summary.duration", new Long(900));

    Assert.assertEquals(expected, OpenTSDBValueConverter.extractMeasuresFromComplexType(measureName, jsonValue));
  }

  @Test
  public void testMetricNameSetting() {
    try {

      final EventMessage em = OpenTSDBDataPointMarshallerTest.populateEM();

      OpenTSDBValueConverter.setMetricsFromSensorType(true);
      final OpenTSDBDataPoint dp = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals("data.testSensorType", dp.getMetric());

      OpenTSDBValueConverter.setMetricsFromSensorType(false);
      final OpenTSDBDataPoint dp2 = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals("data.testProvider.testSensor", dp2.getMetric());

    } catch (final ParseException e) {
      fail("Failed to test data point metric name property setting");
    }
  }

  @Test
  public void testMetricNameIllegalChar() {
    final EventMessage em = OpenTSDBDataPointMarshallerTest.populateEM();
    em.setProvider("pro@vider");

    OpenTSDBValueConverter.setMetricsFromSensorType(false);
    final String result = OpenTSDBValueConverter.createMetricName(em);

    Assert.assertTrue(!result.contains("@"));

  }
}
