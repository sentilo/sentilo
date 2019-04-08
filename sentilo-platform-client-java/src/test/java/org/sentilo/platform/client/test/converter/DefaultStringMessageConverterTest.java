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
package org.sentilo.platform.client.test.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.enums.SubscribeType;
import org.sentilo.common.exception.MessageNotReadableException;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.AlarmsOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.sentilo.platform.client.core.domain.SubscriptionParams;
import org.sentilo.platform.client.core.domain.SubscriptionsOutputMessage;
import org.sentilo.platform.client.core.domain.factory.SubscribeInputMessageFactory;
import org.springframework.util.CollectionUtils;

public class DefaultStringMessageConverterTest {

  private static final String ALERT_ID = "alert1";
  private static final String ALARM_MESSAGE = "threshold exceeded";
  private static final String PROVIDER_ID = "provider1";
  private static final String SENSOR_ID = "sensor1";
  private static final String ORDER = "stop";
  private static final String ENTITY_ID = "prov1";
  private static final SubscriptionParams subscriptionParams = new SubscriptionParams("http://dev.connecta.cat");

  private StringMessageConverter converter = new DefaultStringMessageConverter();

  @Test
  public void marshallSubscribeInputMessage() throws MessageNotWritableException {
    final SubscribeInputMessage message = SubscribeInputMessageFactory.buildSubscription(SubscribeType.DATA, subscriptionParams);

    final String json = converter.marshal(message.getSubscriptionParams());

    final String body = "{\"endpoint\":\"http://dev.connecta.cat\"}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallOrderInputMessage() throws MessageNotWritableException {
    final OrderInputMessage message = new OrderInputMessage(PROVIDER_ID, SENSOR_ID, new OrderMessage(ORDER));

    final String json = converter.marshal(message.getOrder());

    final String body = "{\"order\":\"stop\"}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallAlarmInputMessage() throws MessageNotWritableException {
    final AlarmInputMessage message = new AlarmInputMessage(ALERT_ID, ALARM_MESSAGE);
    final String json = converter.marshal(message);

    final String body = "{\"alertId\":\"alert1\",\"message\":\"threshold exceeded\"}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void unmarshallAlarmInputMessage() throws MessageNotReadableException {
    final String json =
        "{\"alarms\":[{\"message\":\"threshold exceeded\",\"timestamp\":\"21/02/2013T17:49:24\",\"sender\":\"appDemo\"},{\"message\":\"lower limit exceeded\",\"timestamp\":\"22/02/2013T17:49:24\",\"sender\":\"appDemo\"}]}";

    final AlarmsOutputMessage message = (AlarmsOutputMessage) converter.unmarshal(json, AlarmsOutputMessage.class);

    assertNotNull(message);
    assertEquals(2, message.getAlarms().size());
    assertEquals("appDemo", message.getAlarms().get(0).getSender());

  }

  @Test
  public void marshallCatalogInputMessage() throws MessageNotWritableException {
    final CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID);
    message.setSensors(getSensors());
    final String json = converter.marshal(message);
    final String body =
        "{\"sensors\":[{\"sensor\":\"REC012\",\"description\":\"sensor 12\",\"dataType\":\"number\",\"type\":\"humidity\",\"unit\":\"grams\"},{\"sensor\":\"REC013\",\"description\":\"sensor 13\",\"dataType\":\"number\",\"type\":\"humidity\",\"unit\":\"grams\"}]}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallCatalogProviderDeleteInputMessage() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String json = converter.marshal(message);
    assertEquals("{}", json);
  }

  @Test
  public void marshallCatalogSensorsDeleteInputMessage() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String[] sensors = {"1", "2"};
    message.setSensors(sensors);
    final String json = converter.marshal(message);
    final String expectedJson = "{\"sensors\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void marshallCatalogComponentsDeleteInputMessage() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String[] components = {"1", "2"};
    message.setComponents(components);
    final String json = converter.marshal(message);
    final String expectedJson = "{\"components\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void unmarshallCatalogOutputMessage() {
    final String response =
        "{\"providers\":[{\"provider\":\"C\",\"permission\":\"READ\",\"sensors\":[{\"sensor\":\"MAR_02_20_PM001_1010\",\"description\":\"PM10 Sensor IMI 001\",\"dataType\":\"NUMBER\",\"type\":\"air_quality_pm10\",\"unit\":\"ug/m3\",\"component\":\"air_quality\",\"componentType\":\"generic\"},"
            + "{\"sensor\":\"MAR_02_20_PM001_1012\",\"description\":\"PM10 Sensor IMI 002\",\"dataType\":\"NUMBER\",\"type\":\"air_quality_pm10\",\"unit\":\"ug/m3\",\"component\":\"air_quality\",\"componentType\":\"generic\"}]}]}";
    final CatalogOutputMessage outputMessage = (CatalogOutputMessage) converter.unmarshal(response, CatalogOutputMessage.class);
    assertNotNull(outputMessage);
    assertTrue(!CollectionUtils.isEmpty(outputMessage.getProviders()));
    assertTrue(outputMessage.getProviders().size() == 1);
    assertTrue(outputMessage.getProviders().get(0).getSensors().size() == 2);

  }

  @Test
  public void marshallCatalogAlertInputMessage() throws MessageNotWritableException {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    message.setAlerts(getAlerts());
    final String json = converter.marshal(message);
    final String body =
        "{\"alerts\":[{\"id\":\"ALERT012\",\"name\":\"ALERT012\",\"description\":\"alert 12\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"},"
            + "{\"id\":\"ALERT013\",\"name\":\"ALERT013\",\"description\":\"alert 13\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"}]}";

    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallDeleteCatalogEntityAlertInputMessage() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    final String json = converter.marshal(message);
    assertEquals("{}", json);
  }

  @Test
  public void marshallDeleteCatalogAlertsInputMessage() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    final String[] alertsIds = {"1", "2"};
    message.setAlertsIds(alertsIds);
    final String json = converter.marshal(message);
    final String expectedJson = "{\"alertsIds\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void unmarshallCatalogAlertOutputMessage() {
    final String response =
        "{\"alerts\":[{\"id\":\"ALERT012\",\"name\":\"ALERT012\",\"description\":\"alert 12\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"},"
            + "{\"id\":\"ALERT013\",\"name\":\"ALERT013\",\"description\":\"alert 13\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"}]}";
    final CatalogAlertOutputMessage outputMessage = (CatalogAlertOutputMessage) converter.unmarshal(response, CatalogAlertOutputMessage.class);
    assertNotNull(outputMessage);
    assertTrue(!CollectionUtils.isEmpty(outputMessage.getAlerts()));
    assertEquals(2, outputMessage.getAlerts().size());
    assertEquals("ALERT012", outputMessage.getAlerts().get(0).getId());

  }

  @Test
  public void unmarshallSubscriptionsOutputMessage() {
    final String response =
        "{\"subscriptions\":[{\"endpoint\":\"http://www.sentiloendpoint.io/periko.php\",\"type\":\"DATA\",\"provider\":\"MADMAX*\",\"maxRetries\":3,\"retryDelay\":5},"
            + "                  {\"endpoint\":\"http://www.sentiloendpoint.io/periko.php\",\"type\":\"ORDER\",\"provider\":\"CINERETICS\",\"sensor\":\"sensor001\",\"maxRetries\":3,\"retryDelay\":5},"
            + "                  {\"endpoint\":\"http://www.sentiloendpoint.io/periko.php\",\"type\":\"ALARM\",\"alert\":\"ALERT-001\",\"maxRetries\":3,\"retryDelay\":5}]}";
    final SubscriptionsOutputMessage outputMessage = (SubscriptionsOutputMessage) converter.unmarshal(response, SubscriptionsOutputMessage.class);
    assertNotNull(outputMessage);
    assertTrue(!CollectionUtils.isEmpty(outputMessage.getSubscriptions()));
    assertEquals(3, outputMessage.getSubscriptions().size());
    assertEquals("http://www.sentiloendpoint.io/periko.php", outputMessage.getSubscriptions().get(0).getEndpoint());

  }

  private List<CatalogAlert> getAlerts() {
    final List<CatalogAlert> alerts = new ArrayList<CatalogAlert>();
    final CatalogAlert alert1 = buildAlert("ALERT012", "ALERT012", "alert 12", ENTITY_ID);
    final CatalogAlert alert2 = buildAlert("ALERT013", "ALERT013", "alert 13", ENTITY_ID);
    alerts.add(alert1);
    alerts.add(alert2);

    return alerts;
  }

  private CatalogAlert buildAlert(final String id, final String name, final String description, final String entity) {

    final CatalogAlert alert = new CatalogAlert();
    alert.setId(id);
    alert.setName(name);
    alert.setDescription(description);
    alert.setEntity(entity);
    alert.setType("EXTERNAL");

    return alert;
  }

  private List<CatalogSensor> getSensors() {
    final List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
    final CatalogSensor sensor1 = buildSensor("REC012", PROVIDER_ID, "sensor 12", "number", "humidity", "grams");
    final CatalogSensor sensor2 = buildSensor("REC013", PROVIDER_ID, "sensor 13", "number", "humidity", "grams");
    sensors.add(sensor1);
    sensors.add(sensor2);

    return sensors;
  }

  private CatalogSensor buildSensor(final String sensor, final String provider, final String description, final String dataType, final String type,
      final String unit) {
    return buildSensor(sensor, provider, description, dataType, null, type, unit);
  }

  private CatalogSensor buildSensor(final String sensor, final String provider, final String description, final String dataType,
      final String location, final String type, final String unit) {
    final CatalogSensor catalogSensor = new CatalogSensor();
    catalogSensor.setSensor(sensor);
    catalogSensor.setDescription(description);
    catalogSensor.setDataType(dataType);
    catalogSensor.setLocation(location);
    catalogSensor.setType(type);
    catalogSensor.setUnit(unit);

    return catalogSensor;
  }
}
