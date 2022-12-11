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
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.historian.domain.OpenTSDBDataPoint;
import org.sentilo.agent.historian.domain.OpenTSDBDataPoint.Tags;
import org.sentilo.agent.historian.utils.OpenTSDBDataPointMarshaller;
import org.sentilo.agent.historian.utils.OpenTSDBValueConverter;
import org.sentilo.common.domain.EventMessage;

public class OpenTSDBDataPointMarshallerTest {

  private static Long testTime = System.currentTimeMillis();
  private static Long testPublishedAt = testTime + 10;

  @Test
  public void testUnmarshallingValues() {

    try {
      final EventMessage em = populateEM();

      final OpenTSDBDataPoint dp = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals(em.getMessage(), dp.getValue().toString());
      final Map<String, String> tags = dp.getTags();
      Assert.assertEquals(em.getType(), tags.get(Tags.type.name()));
      Assert.assertEquals(em.getSensor(), tags.get(Tags.sensor.name()));
      Assert.assertEquals(em.getProvider(), tags.get(Tags.provider.name()));
      Assert.assertEquals(em.getComponent(), tags.get(Tags.component.name()));
      Assert.assertEquals(em.getAlertType(), tags.get(Tags.alertType.name()));
      Assert.assertEquals(em.getSensorType(), tags.get(Tags.sensorType.name()));
      Assert.assertEquals(em.getPublisher(), tags.get(Tags.publisher.name()));
      Assert.assertEquals(em.getPublisherTenant(), tags.get(Tags.publisherTenant.name()));
      Assert.assertEquals(em.getTenant(), tags.get(Tags.tenant.name()));

    } catch (final ParseException e) {
      fail("Conversion between datapoint and eventmessage should produce same properties");
    }
  }

  @Test
  public void testMetricNameSetting() {
    try {

      final EventMessage em = populateEM();

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
  public void testPublishedAtSetting() {
    try {

      final EventMessage em = populateEM();

      OpenTSDBDataPointMarshaller.setUsePublishedAtTimestamp(false);
      final OpenTSDBDataPoint dp = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals(testTime, dp.getTimestamp());

      OpenTSDBDataPointMarshaller.setUsePublishedAtTimestamp(true);
      final OpenTSDBDataPoint dp2 = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals(testPublishedAt, dp2.getTimestamp());

      em.setPublishedAt(null);
      OpenTSDBDataPointMarshaller.setUsePublishedAtTimestamp(true);
      final OpenTSDBDataPoint dp3 = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals(testTime, dp3.getTimestamp());

    } catch (final ParseException e) {
      fail("Failed to test data point time property setting");
    }
  }

  @Test
  public void testTagsIllegalChars() {
    try {

      final EventMessage em = populateEM();
      em.setProvider("test@provider");
      em.setPublisher("test@publisher");

      OpenTSDBDataPointMarshaller.setUsePublishedAtTimestamp(false);
      final OpenTSDBDataPoint dp = OpenTSDBDataPointMarshaller.unmarshal(em);

      Assert.assertEquals("test.provider", dp.getTags().get(Tags.provider.name()));
      Assert.assertEquals("test.publisher", dp.getTags().get(Tags.publisher.name()));
    } catch (final ParseException e) {
      fail("Failed to test data point tags setting");
    }

  }

  public static EventMessage populateEM() {
    final EventMessage em = new EventMessage();
    em.setTime(testTime);
    em.setTopic("/data/testprovider/testsensor");
    em.setMessage("12345");
    em.setType("DATA");
    em.setSensor("testSensor");
    em.setProvider("testProvider");
    em.setComponent("testComponent");
    em.setAlertType("testAlertType");
    em.setSensorType("testSensorType");
    em.setPublisher("testPublisher");
    em.setPublisherTenant("testPublisherTenant");
    em.setTenant("testTenant");
    em.setPublishedAt(testPublishedAt);

    return em;
  };

}
