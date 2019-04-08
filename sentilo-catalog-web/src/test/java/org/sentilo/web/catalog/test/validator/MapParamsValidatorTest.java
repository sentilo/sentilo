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

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.MapParams;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.validator.LngLatValidator;
import org.sentilo.web.catalog.validator.MapParamsValidator;
import org.springframework.validation.Errors;

public class MapParamsValidatorTest extends AbstractBaseTest {

  @InjectMocks
  private MapParamsValidator mapParamsValidator;

  @Mock
  private LngLatValidator lngLatValidator;

  @Mock
  private Errors errors;

  @Mock
  private Tenant tenant;

  @Mock
  private MapParams mapParams;

  @Mock
  private LngLat lngLat;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void supports() {
    Assert.assertTrue(mapParamsValidator.supports(Tenant.class));
    Assert.assertFalse(mapParamsValidator.supports(MapParams.class));
  }

  @Test
  public void validateZoomLevelOutOfRangeUp() {
    when(tenant.getMapParams()).thenReturn(mapParams);
    when(lngLat.getLatitude()).thenReturn(null);
    when(lngLat.getLongitude()).thenReturn(null);
    when(mapParams.getCenter()).thenReturn(lngLat);
    when(mapParams.getZoomLevel()).thenReturn(22);

    mapParamsValidator.validate(tenant, errors);

    verify(errors, times(1)).rejectValue(anyString(), anyString());
  }

  @Test
  public void validateZoomLevelOutOfRangeDown() {
    when(tenant.getMapParams()).thenReturn(mapParams);
    when(lngLat.getLatitude()).thenReturn(null);
    when(lngLat.getLongitude()).thenReturn(null);
    when(mapParams.getCenter()).thenReturn(lngLat);
    when(mapParams.getZoomLevel()).thenReturn(-1);

    mapParamsValidator.validate(tenant, errors);

    verify(errors, times(1)).rejectValue(anyString(), anyString());
  }

}
