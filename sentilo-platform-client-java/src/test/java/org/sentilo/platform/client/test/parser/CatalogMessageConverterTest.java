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
package org.sentilo.platform.client.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.parser.CatalogMessageConverter;
import org.springframework.util.CollectionUtils;

public class CatalogMessageConverterTest {

  static final String PROVIDER_ID = "prov1";

  CatalogMessageConverter converter = new CatalogMessageConverter();

  @Test
  public void buildBody() throws MessageNotWritableException {
    final CatalogInputMessage message = new CatalogInputMessage(PROVIDER_ID);
    message.setSensors(getSensors());
    final String json = converter.marshall(message);
    final String body =
        "{\"sensors\":[{\"sensor\":\"REC012\",\"description\":\"sensor 12\",\"dataType\":\"number\",\"type\":\"humidity\",\"unit\":\"grams\"},{\"sensor\":\"REC013\",\"description\":\"sensor 13\",\"dataType\":\"number\",\"type\":\"humidity\",\"unit\":\"grams\"}]}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallProviderDeleteRequest() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String json = converter.marshall(message);
    assertEquals("{}", json);
  }

  @Test
  public void marshallSensorsDeleteRequest() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String[] sensors = {"1", "2"};
    message.setSensors(sensors);
    final String json = converter.marshall(message);
    final String expectedJson = "{\"sensors\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void marshallComponentsDeleteRequest() {
    final CatalogDeleteInputMessage message = new CatalogDeleteInputMessage(PROVIDER_ID);
    final String[] components = {"1", "2"};
    message.setComponents(components);
    final String json = converter.marshall(message);
    final String expectedJson = "{\"components\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void unmarshall() {
    final String response =
        "{\"providers\":[{\"provider\":\"C\",\"permission\":\"READ\",\"sensors\":[{\"sensor\":\"MAR_02_20_PM001_1010\",\"description\":\"PM10 Sensor IMI 001\",\"dataType\":\"NUMBER\",\"type\":\"air_quality_pm10\",\"unit\":\"ug/m3\",\"component\":\"air_quality\",\"componentType\":\"generic\"},"
            + "{\"sensor\":\"MAR_02_20_PM001_1012\",\"description\":\"PM10 Sensor IMI 002\",\"dataType\":\"NUMBER\",\"type\":\"air_quality_pm10\",\"unit\":\"ug/m3\",\"component\":\"air_quality\",\"componentType\":\"generic\"}]}]}";
    final CatalogOutputMessage outputMessage = converter.unmarshall(response);
    assertNotNull(outputMessage);
    assertTrue(!CollectionUtils.isEmpty(outputMessage.getProviders()));
    assertTrue(outputMessage.getProviders().size() == 1);
    assertTrue(outputMessage.getProviders().get(0).getSensors().size() == 2);

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
