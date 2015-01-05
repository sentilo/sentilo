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
package org.sentilo.web.catalog.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.domain.CatalogProvider;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.parser.PlatformMessageParser;

public class PlatformMessageParserTest {

  private final PlatformMessageParser parser = new PlatformMessageParser();

  @Test
  public void unmarshallEmptyStatsMessage() {
    final String message = "{\"events\":{},\"performance\":{}}";
    final PlatformStatsMessage stats = parser.unmarshallStatsMessage(message);

    assertNotNull(stats.getEvents());
    assertNotNull(stats.getPerformance());
    assertNull(stats.getEvents().getAlarms());
    assertNull(stats.getEvents().getObservations());
    assertNull(stats.getEvents().getOrders());
    assertNull(stats.getEvents().getTotal());
    assertNull(stats.getPerformance().getDailyAvg());
    assertNull(stats.getPerformance().getInstantAvg());
    assertNull(stats.getPerformance().getMaxAvg());

  }

  @Test
  public void unmarshallStatsMessage() {
    final Long longValue = new Long(3);
    final Long totalValue = new Long(9);
    final String message =
        "{\"events\":{\"alarms\":\"3\",\"observations\":\"3\",\"orders\":\"3\",\"total\":\"9\"},"
            + "\"performance\":{\"dailyAvg\":\"23.4\",\"instantAvg\":\"3.4\", \"maxAvg\":\"323\"}}";

    final PlatformStatsMessage stats = parser.unmarshallStatsMessage(message);

    assertNotNull(stats.getEvents());
    assertNotNull(stats.getPerformance());
    assertEquals(longValue, stats.getEvents().getAlarms());
    assertEquals(longValue, stats.getEvents().getObservations());
    assertEquals(longValue, stats.getEvents().getOrders());
    assertEquals(totalValue, stats.getEvents().getTotal());
    assertEquals(new Float("23.4"), stats.getPerformance().getDailyAvg());
    assertEquals(new Float("3.4"), stats.getPerformance().getInstantAvg());
    assertEquals(new Float("323"), stats.getPerformance().getMaxAvg());
  }

  @Test
  public void marshallSensors() {
    final String jsonExpected =
        "{\"sensors\":[{\"sensor\":\"sensor1\",\"provider\":\"provider1\"},{\"sensor\":\"sensor2\",\"provider\":\"provider1\"}]}";

    final List<CatalogSensor> sensors = buildCatalogSensorList();
    final PlatformAdminInputMessage message = new PlatformAdminInputMessage();
    message.setSensors(sensors);
    assertEquals(jsonExpected, parser.marshall(message));
  }

  @Test
  public void marshallProviders() {
    final String jsonExpected = "{\"providers\":[{\"provider\":\"provider1\"}]}";

    final List<CatalogProvider> providers = buildCatalogProviderList();
    final PlatformAdminInputMessage message = new PlatformAdminInputMessage();
    message.setProviders(providers);
    assertEquals(jsonExpected, parser.marshall(message));
  }

  @Test
  public void marshallEmptyMessage() {
    final String jsonExpected = "{}";
    final PlatformAdminInputMessage message = new PlatformAdminInputMessage();

    assertEquals(jsonExpected, parser.marshall(message));
  }

  private List<CatalogProvider> buildCatalogProviderList() {
    final List<CatalogProvider> providers = new ArrayList<CatalogProvider>();

    final CatalogProvider provider = new CatalogProvider();
    provider.setProvider("provider1");

    providers.add(provider);

    return providers;
  }

  private List<CatalogSensor> buildCatalogSensorList() {
    final List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();

    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setProvider("provider1");
    sensor1.setSensor("sensor1");

    final CatalogSensor sensor2 = new CatalogSensor();
    sensor2.setProvider("provider1");
    sensor2.setSensor("sensor2");

    sensors.add(sensor1);
    sensors.add(sensor2);

    return sensors;
  }

}
