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
package org.sentilo.web.catalog.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.PlatformAdminInputMessage;
import org.sentilo.web.catalog.domain.PlatformStatsMessage;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;

public class PlatformMessageConverterTest {

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  @Test
  public void unmarshalEmptyStatsMessage() {
    final String message = "{\"events\":{},\"performance\":{}}";
    final PlatformStatsMessage stats = (PlatformStatsMessage) converter.unmarshal(message, PlatformStatsMessage.class);

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
  public void unmarshalStatsMessage() {
    final Long longValue = new Long(3);
    final Long totalValue = new Long(9);
    final String message = "{\"events\":{\"alarms\":\"3\",\"observations\":\"3\",\"orders\":\"3\",\"total\":\"9\"},"
        + "\"performance\":{\"dailyAvg\":\"23.4\",\"instantAvg\":\"3.4\", \"maxAvg\":\"323\"}}";

    final PlatformStatsMessage stats = (PlatformStatsMessage) converter.unmarshal(message, PlatformStatsMessage.class);

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
  public void marshalSensors() {
    final String jsonExpected =
        "{\"sensors\":[{\"sensor\":\"sensor1\",\"provider\":\"provider1\",\"state\":\"online\",\"ttl\":10800},{\"sensor\":\"sensor2\",\"provider\":\"provider1\",\"state\":\"offline\",\"ttl\":0}]}";

    final PlatformAdminInputMessage message = new PlatformAdminInputMessage(buildMockSensors());
    assertEquals(jsonExpected, converter.marshal(message));
  }

  @Test
  public void marshalProviders() {
    final String jsonExpected = "{\"providers\":[{\"entityId\":\"provider1\"}]}";

    final PlatformAdminInputMessage message = new PlatformAdminInputMessage(buildMockProviders());
    assertEquals(jsonExpected, converter.marshal(message));
  }

  @Test
  public void marshalApplications() {
    final String jsonExpected = "{\"applications\":[{\"entityId\":\"application_client1\"}]}";

    final PlatformAdminInputMessage message = new PlatformAdminInputMessage(buildMockApplications());
    assertEquals(jsonExpected, converter.marshal(message));
  }

  @Test
  public void marshalEmptyMessage() {
    final String jsonExpected = "{}";
    final PlatformAdminInputMessage message = new PlatformAdminInputMessage();

    assertEquals(jsonExpected, converter.marshal(message));
  }

  private List<Sensor> buildMockSensors() {
    final String[] sensorsIds = {"sensor1", "sensor2"};
    final SensorState[] states = {SensorState.online, SensorState.offline};
    final int[] ttls = {180, 0};
    final List<Sensor> sensors = new ArrayList<Sensor>();

    for (int i = 0; i < sensorsIds.length; i++) {
      final Sensor sensor = new Sensor("provider1", "component1", sensorsIds[i]);
      sensor.setState(states[i]);
      sensor.setTtl(ttls[i]);
      sensors.add(sensor);
    }

    return sensors;
  }

  private List<Provider> buildMockProviders() {
    final Provider[] providers = {new Provider("provider1")};
    return Arrays.asList(providers);
  }

  private List<Application> buildMockApplications() {
    final Application[] applications = {new Application("application_client1")};
    return Arrays.asList(applications);
  }
}
