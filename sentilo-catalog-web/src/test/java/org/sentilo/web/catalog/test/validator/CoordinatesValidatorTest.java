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
package org.sentilo.web.catalog.test.validator;

import static org.mockito.Matchers.anyString;
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
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.validator.CoordinatesValidator;
import org.springframework.validation.Errors;

public class CoordinatesValidatorTest extends AbstractBaseTest {

  @Mock
  private Errors errors;

  @Mock
  private Component component;

  @Mock
  private Location location;

  @InjectMocks
  private CoordinatesValidator validator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(component.getLocation()).thenReturn(location);
  }

  @Test
  public void supports() {
    Assert.assertTrue(validator.supports(Component.class));
    Assert.assertFalse(validator.supports(Sensor.class));
  }

  @Test
  public void validateStaticComponentWithoutLocation() {
    when(location.getCoordinates()).thenReturn(null);
    when(component.isStaticComponent()).thenReturn(true);
    validator.validate(component, errors);

    verify(errors).rejectValue(anyString(), anyString());
  }

  @Test
  public void validateStaticComponent() throws Exception {
    final List<LngLat> coordinates = generateRandomList(LngLat.class);
    final LngLat[] coordinatesArr = new LngLat[coordinates.size()];
    when(location.getCoordinates()).thenReturn(coordinates.toArray(coordinatesArr));
    when(component.isStaticComponent()).thenReturn(true);
    validator.validate(component, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }

  @Test
  public void validateMobileComponentWithoutLocation() {
    when(location.getCoordinates()).thenReturn(null);
    when(component.isStaticComponent()).thenReturn(false);
    validator.validate(component, errors);

    verify(errors, times(0)).rejectValue(anyString(), anyString());
  }
}
