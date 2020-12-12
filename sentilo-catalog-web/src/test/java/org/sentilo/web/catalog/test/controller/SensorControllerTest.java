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
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.controller.admin.SensorController;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.PlatformService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorSubstateService;
import org.sentilo.web.catalog.service.SensorTypesService;
import org.springframework.context.MessageSource;
import org.springframework.validation.BindingResult;
import org.springframework.validation.support.BindingAwareModelMap;
import org.springframework.web.servlet.mvc.support.RedirectAttributesModelMap;

public class SensorControllerTest {

  private static final String SENSOR_NAME = "sensor_name";
  private static final String PROVIDER_ID = "provider_id";
  private static final String COMPONENT_ID = "component_id";
  private static final int TTL_MIN = 5;
  private static final Object CREATE_SENSOR = "redirect:/admin/sensor/list?nameTableRecover=sensorTable&sfbr=false";
  private static final String SENSOR_CREATE_PATH = "/admin/sensor/create";
  private static final String SENSOR_UPDATE_PATH = "/admin/sensor/save";

  private RedirectAttributesModelMap redirectAttributes;
  private BindingAwareModelMap model;

  @InjectMocks
  private SensorController controller;

  @Mock
  private BindingResult result;

  @Mock
  private HttpServletRequest request;

  @Mock
  private SensorService sensorService;

  @Mock
  private ProviderService providerService;

  @Mock
  private ComponentService componentService;

  @Mock
  private SensorTypesService sensorTypeService;

  @Mock
  private SensorSubstateService sensorSubStateService;

  @Mock
  private PlatformService platformService;

  @Mock
  private MessageSource messageSource;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    redirectAttributes = new RedirectAttributesModelMap();
    model = new BindingAwareModelMap();
    model.put("activeMenu", "/sensor");
  }

  @Test
  public void newSensorResource() {
    when(messageSource.getMessage(anyString(), any(Object[].class), any(Locale.class))).thenReturn("1,2,3");
    final String view = controller.newResource(request, model);

    Assert.assertEquals("sensor/sensor_new", view);

  }

  @Test
  public void createSensor() {
    final Sensor sensor = buildSensor();

    when(result.hasErrors()).thenReturn(false);
    when(sensorService.create(sensor)).thenReturn(sensor);
    when(request.getServletPath()).thenReturn(SENSOR_CREATE_PATH);
    final String view = controller.createResource(sensor, result, model, redirectAttributes, request);

    Assert.assertEquals(CREATE_SENSOR, view);
  }

  @Test
  public void doBeforeUpdate() {
    final Sensor oldSensor = Mockito.mock(Sensor.class);
    final Sensor sensor = buildSensor();
    when(result.hasErrors()).thenReturn(false);
    when(platformService.getPlatformTtl()).thenReturn(3600);
    when(sensorService.find(sensor)).thenReturn(oldSensor);
    when(request.getServletPath()).thenReturn(SENSOR_UPDATE_PATH);

    controller.updateResource(sensor, result, SENSOR_NAME, model, redirectAttributes, request);

    verify(sensorService).find(sensor);
    verify(oldSensor).getAdditionalInfo();

  }

  private Sensor buildSensor() {
    final Sensor sensor = new Sensor();
    sensor.setId(SENSOR_NAME);
    sensor.setProviderId(PROVIDER_ID);
    sensor.setComponentId(COMPONENT_ID);
    sensor.setTtl(TTL_MIN);
    return sensor;
  }
}
