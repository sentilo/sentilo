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
package org.sentilo.web.catalog.test.controller;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMapOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.startsWith;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Lock;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.domain.SensorMessageLocation;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.lock.LockFactory;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.controller.api.ApiController;
import org.sentilo.web.catalog.converter.ApiConverter;
import org.sentilo.web.catalog.converter.ApiConverterContext;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.EntitiesMetadataDTO;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CatalogSensorService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.sentilo.web.catalog.validator.ApiValidator;

@RunWith(PowerMockRunner.class)
@PrepareForTest({ApiConverter.class})
public class ApiControllerTest extends AbstractBaseTest {

  private final String providerId = "mockProviderId";

  @InjectMocks
  private ApiController controller;

  @Mock
  private ComponentService componentService;
  @Mock
  private SensorService sensorService;
  @Mock
  private CatalogInputMessage message;
  @Mock
  private CatalogDeleteInputMessage deleteMessage;
  @Mock
  private ProviderService providerService;
  @Mock
  private ApplicationService applicationService;
  @Mock
  PermissionService permissionService;
  @Mock
  private ApiValidator validator;
  @Mock
  private ApiValidationResults validationResult;
  @Mock
  private CatalogSensorService catalogSensorService;
  @Mock
  private LockFactory lockProvider;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void updateMobileComponentsLocation() throws Exception {
    final List<SensorMessageLocation> resources = generateRandomSensorLocationList();
    final Lock lock = Mockito.mock(Lock.class);
    final Sensor sensor10 = new Sensor("mockProviderId.component1.sensor_10");
    final Sensor sensor11 = new Sensor("mockProviderId.component1.sensor_11");
    final Sensor sensor2 = new Sensor("mockProviderId.component2.sensor_21");
    final Component component1 = new Component("mockProviderId.component1");
    final Component component2 = new Component("mockProviderId.component2");
    when(message.getLocations()).thenReturn(resources);
    when(sensorService.findByName(eq(providerId), startsWith("sensor_10"))).thenReturn(sensor10);
    when(sensorService.findByName(eq(providerId), startsWith("sensor_11"))).thenReturn(sensor11);
    when(sensorService.findByName(eq(providerId), startsWith("sensor_2"))).thenReturn(sensor2);
    when(componentService.findById("mockProviderId.component1")).thenReturn(component1);
    when(componentService.findById("mockProviderId.component2")).thenReturn(component2);
    when(lockProvider.getLock("catalog_update_components_locations")).thenReturn(lock);

    final CatalogResponseMessage response = controller.updateMobileComponentsLocation(message);

    verify(componentService).updateAll(anyCollectionOf(Component.class));
    verify(lock).lock();
    verify(lock).unlock();
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void updateMobileComponentsLocationWithError() throws Exception {
    final List<SensorMessageLocation> resources = generateRandomList(SensorMessageLocation.class);
    final Lock lock = Mockito.mock(Lock.class);
    when(message.getLocations()).thenReturn(resources);
    when(lockProvider.getLock("catalog_update_components_locations")).thenReturn(lock);
    doThrow(Exception.class).when(sensorService).findByName(anyString(), anyString());

    final CatalogResponseMessage response = controller.updateMobileComponentsLocation(message);

    verify(componentService, times(0)).updateAll(anyCollectionOf(Component.class));
    verify(lock).lock();
    verify(lock).unlock();
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void getPermissions() {
    controller.getPermissions();
    verify(permissionService).retrievePermissions();
  }

  @Test
  public void getEntitiesMetadata() throws Exception {
    final List<Application> applications = generateRandomList(Application.class);
    final List<Provider> providers = generateRandomList(Provider.class);
    when(applicationService.findAll()).thenReturn(applications);
    when(providerService.findAll()).thenReturn(providers);

    final EntitiesMetadataDTO entitiesMetadata = controller.getEntitiesMetadata();

    Assert.assertEquals(applications.size() + providers.size(), entitiesMetadata.getEntitiesMetadata().size());
  }

  @Test
  public void registerSensors() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(false);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);
    when(validator.validateSensorsAndComponents(anyListOf(Sensor.class), anyListOf(Component.class), eq(false))).thenReturn(validationResult);

    final CatalogResponseMessage response = controller.registerSensors(message, providerId);

    verify(validator).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(false));
    verify(componentService).insertAll(argThat(new EqualListSizeQueryMatcher<Component>(components.size())));
    verify(sensorService).insertAll(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void registerSensors_withFieldFormatErrors() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(true);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);

    final CatalogResponseMessage response = controller.registerSensors(message, providerId);

    verify(validator, times(0)).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(false));
    Assert.assertEquals(CatalogResponseMessage.BAD_REQUEST, response.getCode());
  }

  @Test
  public void registerSensors_withInternalError() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(false);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);
    when(validator.validateSensorsAndComponents(anyListOf(Sensor.class), anyListOf(Component.class), eq(false))).thenReturn(validationResult);
    doThrow(new RuntimeException("Connection timed-out")).when(componentService).insertAll(anyListOf(Component.class));

    final CatalogResponseMessage response = controller.registerSensors(message, providerId);

    verify(validator).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(false));
    verify(componentService).insertAll(argThat(new EqualListSizeQueryMatcher<Component>(components.size())));
    verify(sensorService, times(0)).insertAll(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())));
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void updateComponentOrSensors() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(false);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);
    when(validator.validateSensorsAndComponents(anyListOf(Sensor.class), anyListOf(Component.class), eq(true))).thenReturn(validationResult);

    final CatalogResponseMessage response = controller.updateComponentOrSensors(message, providerId);

    verify(validator).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(true));
    verify(componentService).updateAll(argThat(new EqualListSizeQueryMatcher<Component>(components.size())));
    verify(sensorService).updateAll(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void updateComponentOrSensors_withFieldFormatErrors() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(true);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);

    final CatalogResponseMessage response = controller.updateComponentOrSensors(message, providerId);

    verify(validator, times(0)).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(true));
    Assert.assertEquals(CatalogResponseMessage.BAD_REQUEST, response.getCode());
  }

  @Test
  public void updateComponentOrSensors_withInternalError() throws Exception {
    PowerMockito.mockStatic(ApiConverter.class);
    final Provider provider = new Provider(providerId);
    final List<Sensor> sensors = generateRandomList(Sensor.class);
    final List<Component> components = generateRandomList(Component.class);

    when(providerService.find(eq(provider))).thenReturn(provider);
    when(validator.validateFieldFormatValues(any(ApiConverterContext.class))).thenReturn(validationResult);
    when(validationResult.hasErrors()).thenReturn(false);
    when(ApiConverter.buildComponentsFromCatalogComponents(any(ApiConverterContext.class))).thenReturn(components);
    when(ApiConverter.buildSensorsFromCatalogSensors(any(ApiConverterContext.class))).thenReturn(sensors);
    when(validator.validateSensorsAndComponents(anyListOf(Sensor.class), anyListOf(Component.class), eq(true))).thenReturn(validationResult);
    doThrow(new RuntimeException("Connection timed-out")).when(componentService).updateAll(anyListOf(Component.class));

    final CatalogResponseMessage response = controller.updateComponentOrSensors(message, providerId);

    verify(validator).validateSensorsAndComponents(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())),
        argThat(new EqualListSizeQueryMatcher<Component>(components.size())), eq(true));
    verify(componentService).updateAll(argThat(new EqualListSizeQueryMatcher<Component>(components.size())));
    verify(sensorService, times(0)).updateAll(argThat(new EqualListSizeQueryMatcher<Sensor>(sensors.size())));
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }

  @Test
  public void notAllowedActionException() throws Exception {
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(null);

    final CatalogResponseMessage response_1 = controller.registerSensors(message, providerId);
    final CatalogResponseMessage response_2 = controller.updateComponentOrSensors(message, providerId);
    final CatalogResponseMessage response_3 = controller.deleteProviderChilds(deleteMessage, providerId);

    Assert.assertEquals(CatalogResponseMessage.FORBIDDEN, response_1.getCode());
    Assert.assertEquals(CatalogResponseMessage.FORBIDDEN, response_2.getCode());
    Assert.assertEquals(CatalogResponseMessage.FORBIDDEN, response_3.getCode());
  }

  @Test
  public void getAuthorizedProviders() throws Exception {
    final List<Permission> permissions = generateRandomList(Permission.class);

    when(permissionService.getActivePermissions(providerId)).thenReturn(permissions);
    when(catalogSensorService.getSensorsByProvider(anyString(), anyMapOf(String.class, String.class))).thenReturn(Collections.emptyList());

    final CatalogResponseMessage response = controller.getAuthorizedProviders(providerId, Collections.emptyMap());

    verify(catalogSensorService, times(permissions.size())).getSensorsByProvider(anyString(), anyMapOf(String.class, String.class));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void getAuthorizedProviders_with_InternalServerError() throws Exception {
    final List<Permission> permissions = generateRandomList(Permission.class);

    when(permissionService.getActivePermissions(providerId)).thenReturn(permissions);
    doThrow(new RuntimeException()).when(catalogSensorService).getSensorsByProvider(anyString(), anyMapOf(String.class, String.class));

    final CatalogResponseMessage response = controller.getAuthorizedProviders(providerId, Collections.emptyMap());

    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
    Assert.assertTrue(response.getErrorMessage().contains(SentiloConstants.CATALOG_API_ERROR));
  }

  @Test
  public void deleteProviderChilds_whenNullMessage() {
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(provider);

    final CatalogResponseMessage response = controller.deleteProviderChilds(null, providerId);

    verify(providerService).deleteChildren(eq(provider));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void deleteProviderChilds() {
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(provider);
    when(deleteMessage.getSensorsIds()).thenReturn(new String[] {});
    when(deleteMessage.getComponentsIds()).thenReturn(new String[] {});

    final CatalogResponseMessage response = controller.deleteProviderChilds(deleteMessage, providerId);

    verify(providerService).deleteChildren(eq(provider));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void deleteProviderComponentsChilds() {
    final String[] componentsIds = {"component_1", "component_2"};
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(provider);
    when(deleteMessage.getSensorsIds()).thenReturn(new String[] {});
    when(deleteMessage.getComponentsIds()).thenReturn(componentsIds);

    final CatalogResponseMessage response = controller.deleteProviderChilds(deleteMessage, providerId);

    verify(providerService, times(0)).deleteChildren(eq(provider));
    verify(componentService).deleteComponents(providerId, componentsIds);
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void deleteProviderSensorsChilds() {
    final String[] sensorsIds = {"sensor_1", "sensor_2", "sensor_3"};
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(provider);
    when(deleteMessage.getSensorsIds()).thenReturn(sensorsIds);
    when(deleteMessage.getComponentsIds()).thenReturn(new String[] {});

    final CatalogResponseMessage response = controller.deleteProviderChilds(deleteMessage, providerId);

    verify(providerService, times(0)).deleteChildren(eq(provider));
    verify(sensorService).deleteSensors(providerId, sensorsIds);
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void deleteProviderChilds_withInternalServerError() {
    final Provider provider = new Provider(providerId);
    when(providerService.find(eq(provider))).thenReturn(provider);
    when(deleteMessage.getSensorsIds()).thenReturn(new String[] {});
    when(deleteMessage.getComponentsIds()).thenReturn(new String[] {});
    doThrow(new RuntimeException()).when(providerService).deleteChildren(eq(provider));

    final CatalogResponseMessage response = controller.deleteProviderChilds(deleteMessage, providerId);

    verify(providerService).deleteChildren(eq(provider));
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
    Assert.assertTrue(response.getErrorMessage().contains(SentiloConstants.CATALOG_API_ERROR));
  }

  protected List<SensorMessageLocation> generateRandomSensorLocationList() throws InstantiationException, IllegalAccessException {
    // Retorna una lista de N SensorMessageLocation, que pertenecen a 2 componentes diferentes
    // La lista de sensores incluye casos con localizaciones erroneas, instantes duplicados del
    // mismo sensor, ...
    final EventMessage message = new EventMessage();
    message.setProvider(providerId);
    message.setType(EventType.DATA.name());

    final List<SensorMessageLocation> resources = new ArrayList<SensorMessageLocation>();

    final long initialFromTs = System.currentTimeMillis() - 10 * 3600 * 1000;
    final String[] sensors = {"sensor_10", "sensor_10", "sensor_10", "sensor_10", "sensor_11", "sensor_11", "sensor_21", "sensor_21"};

    final String[] locations = {"2.123456 41.23456", "2.123423 41.23423", "2.123432 41.23432", "2.123456 41.23456", "2.123456 41.23456",
        "2.123456 192.23456", "2.789045 87.23456", "2.789045 87.23456"};

    final long[] times = {initialFromTs, initialFromTs + 1000, initialFromTs + 2000, initialFromTs, initialFromTs, initialFromTs + 1000,
        initialFromTs, initialFromTs + 1000};

    // sensors [10,19] --> component1, sensors [20,29] --> component2
    for (int i = 0; i < sensors.length; i++) {
      final SensorMessageLocation sml = new SensorMessageLocation(message);
      sml.setSensor(sensors[i]);
      sml.setFromTsTime(times[i]);
      sml.setLocation(locations[i]);

      resources.add(sml);
    }

    return resources;
  }

}
