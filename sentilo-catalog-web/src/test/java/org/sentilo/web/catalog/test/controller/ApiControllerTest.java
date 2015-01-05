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
package org.sentilo.web.catalog.test.controller;

import static org.mockito.Matchers.anyCollectionOf;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.SensorLocationElement;
import org.sentilo.web.catalog.controller.api.ApiController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.test.AbstractBaseTest;

public class ApiControllerTest extends AbstractBaseTest {

  @InjectMocks
  private ApiController controller;

  @Mock
  private ComponentService componentService;
  @Mock
  private SensorService sensorService;
  @Mock
  private CatalogInputMessage message;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void updateMobileComponentsLocation() throws Exception {
    final List<SensorLocationElement> resources = generateRandomList(SensorLocationElement.class);
    when(message.getLocations()).thenReturn(resources);

    final CatalogResponseMessage response = controller.updateMobileComponentsLocation(message);

    verify(sensorService, times(resources.size())).findByName(anyString(), anyString());
    verify(componentService).updateAll(anyCollectionOf(Component.class));
    Assert.assertEquals(CatalogResponseMessage.OK, response.getCode());
  }

  @Test
  public void updateMobileComponentsLocationWithError() throws Exception {
    final List<SensorLocationElement> resources = generateRandomList(SensorLocationElement.class);
    when(message.getLocations()).thenReturn(resources);
    doThrow(Exception.class).when(sensorService).findByName(anyString(), anyString());

    final CatalogResponseMessage response = controller.updateMobileComponentsLocation(message);

    verify(componentService, times(0)).updateAll(anyCollectionOf(Component.class));
    Assert.assertEquals(CatalogResponseMessage.INTERNAL_SERVER_ERROR, response.getCode());
  }
}
