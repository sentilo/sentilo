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
package org.sentilo.web.catalog.integration.controller;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.common.domain.CatalogComponent;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.web.catalog.controller.api.ApiController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Permissions;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.EntitiesMetadataDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.CollectionUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ActiveProfiles("dev")
@ContextConfiguration(locations = "classpath:spring/test-mongodb-service-context.xml")
public class ApiControllerIntegrationTest {

  @Autowired
  private ApiController controller;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private SensorService sensorService;

  private static final String NEW_PROV = "new_app_demo_provider";
  private static final String DEMO_APP = "appDemo";
  private static final String MOCK_APP = "mockApp";
  private static final String MOCK_COMPONENT = "mockComponent";
  private static final String MOCK_COMPONENT_2 = "mockComponent2";
  private static final String COMP_DEMO = "comp_demo";
  private static final String MOCK_SENSOR_1 = "mockSensor1";
  private static final String MOCK_SENSOR_2 = "mockSensor2";
  private static final String MOCK_TYPE = "mockType";
  private static final String TEMP_TYPE = "temperature";
  private static final String KELVIN_UNIT = "K";
  private static final String CELSIUS_UNIT = "C";

  @Test
  public void getPermissions() {
    final Permissions permissions = controller.getPermissions();
    assertTrue("Found " + permissions.getPermissions().size() + " and must be greater than 0", permissions.getPermissions().size() >= 1);
  }

  @Test
  public void getAuthorizations() {
    final EntitiesMetadataDTO credentials = controller.getEntitiesMetadata();
    assertTrue("Found " + credentials.getEntitiesMetadata().size() + " and must be greater than 0", credentials.getEntitiesMetadata().size() >= 1);
  }

  @Test
  public void getAuthorizedProviders() {
    final Map<String, String> filterParams = new HashMap<String, String>();
    CatalogResponseMessage response = controller.getAuthorizedProviders(MOCK_APP, filterParams);
    assertTrue(CollectionUtils.isEmpty(response.getProviders()));

    response = controller.getAuthorizedProviders(DEMO_APP, filterParams);
    assertTrue(!CollectionUtils.isEmpty(response.getProviders()));

    filterParams.put("type", MOCK_TYPE);
    response = controller.getAuthorizedProviders(DEMO_APP, filterParams);
    assertTrue(CollectionUtils.isEmpty(response.getProviders()));

    filterParams.clear();
    filterParams.put("type", TEMP_TYPE);
    response = controller.getAuthorizedProviders(DEMO_APP, filterParams);
    assertTrue(!CollectionUtils.isEmpty(response.getProviders()));

    filterParams.clear();
    filterParams.put("type", TEMP_TYPE);
    filterParams.put("component", MOCK_COMPONENT);
    response = controller.getAuthorizedProviders(DEMO_APP, filterParams);
    assertTrue(CollectionUtils.isEmpty(response.getProviders()));

    filterParams.clear();
    filterParams.put("type", TEMP_TYPE);
    filterParams.put("component", COMP_DEMO);
    response = controller.getAuthorizedProviders(DEMO_APP, filterParams);
    assertTrue(!CollectionUtils.isEmpty(response.getProviders()));

  }

  @Test
  public void registerUpdateAndDeleteSensors() {
    final String componentId = Component.buildId(NEW_PROV, MOCK_COMPONENT);

    final SearchFilter filterByProvider = new SearchFilter();
    filterByProvider.addAndParam("providerId", NEW_PROV);

    final SearchFilter filterByProviderAndComponent = new SearchFilter();
    filterByProviderAndComponent.addAndParam("providerId", NEW_PROV);
    filterByProviderAndComponent.addAndParam("componentId", componentId);

    // Inicializamos contexto
    doDelete(filterByProvider);

    // Insert
    doInsert(filterByProvider, filterByProviderAndComponent);

    // Update sensors
    doSensorsUpdate(filterByProvider, filterByProviderAndComponent, componentId);

    // Update components
    doComponentsUpdate(filterByProvider, filterByProviderAndComponent, componentId);

    // Delete resources
    doDelete(filterByProvider);

  }

  @Test
  public void registerUpdateAndDeleteInvalidSensors() {
    final String componentId = Component.buildId(NEW_PROV, MOCK_COMPONENT);

    final SearchFilter filterByProvider = new SearchFilter();
    filterByProvider.addAndParam("providerId", NEW_PROV);

    final SearchFilter filterByProviderAndComponent = new SearchFilter();
    filterByProviderAndComponent.addAndParam("providerId", NEW_PROV);
    filterByProviderAndComponent.addAndParam("componentId", componentId);

    // Inicializamos contexto
    doDelete(filterByProvider);

    // Insert
    doInsert(filterByProvider, filterByProviderAndComponent);

    doInvalidSensorsInsert();

    // Update sensors
    doInvalidSensorsUpdate();

    // Update components
    doInvalidComponentsUpdate(filterByProvider, filterByProviderAndComponent, componentId);

    // Delete resources
    doDelete(filterByProvider);

  }

  private void doDelete(final SearchFilter filterByProvider) {
    controller.deleteProviderChilds(null, NEW_PROV);
    assertTrue(CollectionUtils.isEmpty(componentService.search(filterByProvider).getContent()));
    assertTrue(CollectionUtils.isEmpty(sensorService.search(filterByProvider).getContent()));
  }

  private void doInsert(final SearchFilter filterByProvider, final SearchFilter filterByProviderAndComponent) {
    final CatalogInputMessage inputMessage = getSensorsAndComponentsToInsert();
    controller.registerSensors(inputMessage, NEW_PROV);
    final List<Component> components = componentService.search(filterByProvider).getContent();
    assertTrue(!CollectionUtils.isEmpty(components));
    assertTrue(components.size() == 1);
    assertNull(components.get(0).getDescription());

    List<Sensor> sensors = sensorService.search(filterByProvider).getContent();
    assertTrue(!CollectionUtils.isEmpty(sensors));
    assertTrue(sensors.size() == 2);

    sensors = sensorService.search(filterByProviderAndComponent).getContent();
    assertTrue(!CollectionUtils.isEmpty(sensors));
    assertTrue(sensors.size() == 2);
  }

  private void doInvalidSensorsInsert() {
    CatalogInputMessage inputMessage = getDuplicateSensorsToInsert();
    CatalogResponseMessage response = controller.registerSensors(inputMessage, NEW_PROV);
    assertTrue(CatalogResponseMessage.BAD_REQUEST.equals(response.getCode()));

    inputMessage = getAlreadyExistSensorToInsert();
    response = controller.registerSensors(inputMessage, NEW_PROV);
    assertTrue(CatalogResponseMessage.BAD_REQUEST.equals(response.getCode()));
  }

  private void doSensorsUpdate(final SearchFilter filterByProvider, final SearchFilter filterByProviderAndComponent, final String componentId) {
    final CatalogInputMessage inputMessage = getSensorsToUpdate();
    controller.updateComponentOrSensors(inputMessage, NEW_PROV);
    final List<Component> components = componentService.search(filterByProvider).getContent();
    assertTrue(!CollectionUtils.isEmpty(components));
    assertTrue(components.size() == 1);
    assertNull(components.get(0).getDescription());

    final List<Sensor> sensors = sensorService.search(filterByProvider).getContent();
    assertTrue(!CollectionUtils.isEmpty(sensors));
    assertTrue(sensors.size() == 2);

    final Sensor sensor1 = sensorService.find(new Sensor(Sensor.buildId(componentId, MOCK_SENSOR_1)));
    final Sensor sensor2 = sensorService.find(new Sensor(Sensor.buildId(componentId, MOCK_SENSOR_2)));
    assertTrue(sensor1 != null && KELVIN_UNIT.equals(sensor1.getUnit()));
    assertTrue(sensor2 != null && CELSIUS_UNIT.equals(sensor2.getUnit()));
  }

  private void doInvalidSensorsUpdate() {
    final CatalogInputMessage inputMessage = getInvalidSensorsToUpdate();
    final CatalogResponseMessage response = controller.updateComponentOrSensors(inputMessage, NEW_PROV);
    assertTrue(CatalogResponseMessage.BAD_REQUEST.equals(response.getCode()));
  }

  private void doComponentsUpdate(final SearchFilter filterByProvider, final SearchFilter filterByProviderAndComponent, final String componentId) {
    final CatalogInputMessage inputMessage = getComponentsToUpdate();
    controller.updateComponentOrSensors(inputMessage, NEW_PROV);
    final List<Component> components = componentService.search(filterByProvider).getContent();
    assertTrue(!CollectionUtils.isEmpty(components));
    assertTrue(components.size() == 1);
    assertNotNull(components.get(0).getDescription());
  }

  private void doInvalidComponentsUpdate(final SearchFilter filterByProvider, final SearchFilter filterByProviderAndComponent,
      final String componentId) {
    final CatalogInputMessage inputMessage = getInvalidComponentsToUpdate();
    final CatalogResponseMessage response = controller.updateComponentOrSensors(inputMessage, NEW_PROV);
    assertTrue(CatalogResponseMessage.BAD_REQUEST.equals(response.getCode()));
  }

  private CatalogInputMessage getSensorsAndComponentsToInsert() {
    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setComponent(MOCK_COMPONENT);
    sensor1.setSensor(MOCK_SENSOR_1);
    sensor1.setType(TEMP_TYPE);
    sensor1.setUnit(CELSIUS_UNIT);

    final CatalogSensor sensor2 = new CatalogSensor();
    sensor2.setComponent(MOCK_COMPONENT);
    sensor2.setSensor(MOCK_SENSOR_2);
    sensor2.setType(TEMP_TYPE);
    sensor2.setUnit(CELSIUS_UNIT);

    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    catalogSensors.add(sensor1);
    catalogSensors.add(sensor2);

    inputMessage.setSensors(catalogSensors);

    return inputMessage;
  }

  private CatalogInputMessage getDuplicateSensorsToInsert() {
    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setComponent(MOCK_COMPONENT);
    sensor1.setSensor(MOCK_SENSOR_1);
    sensor1.setType(TEMP_TYPE);
    sensor1.setUnit(CELSIUS_UNIT);

    final CatalogSensor sensor2 = new CatalogSensor();
    sensor2.setComponent(MOCK_COMPONENT);
    sensor2.setSensor(MOCK_SENSOR_1);
    sensor2.setType(TEMP_TYPE);
    sensor2.setUnit(CELSIUS_UNIT);

    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    catalogSensors.add(sensor1);
    catalogSensors.add(sensor2);

    inputMessage.setSensors(catalogSensors);

    return inputMessage;
  }

  private CatalogInputMessage getAlreadyExistSensorToInsert() {
    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setComponent(MOCK_COMPONENT);
    sensor1.setSensor(MOCK_SENSOR_1);
    sensor1.setType(TEMP_TYPE);
    sensor1.setUnit(CELSIUS_UNIT);

    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    catalogSensors.add(sensor1);

    inputMessage.setSensors(catalogSensors);

    return inputMessage;
  }

  private CatalogInputMessage getSensorsToUpdate() {

    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setComponent(MOCK_COMPONENT);
    sensor1.setSensor(MOCK_SENSOR_1);
    sensor1.setUnit(KELVIN_UNIT);

    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    catalogSensors.add(sensor1);

    inputMessage.setSensors(catalogSensors);

    return inputMessage;
  }

  private CatalogInputMessage getInvalidSensorsToUpdate() {

    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogSensor sensor1 = new CatalogSensor();
    sensor1.setComponent(MOCK_COMPONENT);
    sensor1.setSensor(MOCK_SENSOR_1);
    sensor1.setType(MOCK_TYPE);

    final List<CatalogSensor> catalogSensors = new ArrayList<CatalogSensor>();
    catalogSensors.add(sensor1);

    inputMessage.setSensors(catalogSensors);

    return inputMessage;
  }

  private CatalogInputMessage getComponentsToUpdate() {
    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogComponent component1 = new CatalogComponent();
    component1.setComponent(MOCK_COMPONENT);
    component1.setComponentDesc("mock description");

    // Este sensor realmente no se actualizara ya que no ha sido insertado previamente
    final CatalogComponent component2 = new CatalogComponent();
    component2.setComponent(MOCK_COMPONENT_2);
    component2.setComponentDesc("mock description");

    final List<CatalogComponent> catalogComponents = new ArrayList<CatalogComponent>();
    catalogComponents.add(component1);
    catalogComponents.add(component2);

    inputMessage.setComponents(catalogComponents);

    return inputMessage;
  }

  private CatalogInputMessage getInvalidComponentsToUpdate() {
    final CatalogInputMessage inputMessage = new CatalogInputMessage();
    final CatalogComponent component1 = new CatalogComponent();
    component1.setComponent(MOCK_COMPONENT);
    component1.setComponentType(MOCK_TYPE);

    final List<CatalogComponent> catalogComponents = new ArrayList<CatalogComponent>();
    catalogComponents.add(component1);

    inputMessage.setComponents(catalogComponents);

    return inputMessage;
  }

}
