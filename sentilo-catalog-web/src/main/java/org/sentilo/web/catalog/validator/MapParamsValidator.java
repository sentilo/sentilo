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
package org.sentilo.web.catalog.validator;

import org.sentilo.web.catalog.domain.MapParams;
import org.sentilo.web.catalog.domain.Tenant;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class MapParamsValidator implements Validator {

  private static final int MIN_ZOOM_LEVEL = 0;

  private static final int MAX_ZOOM_LEVEL = 21;

  @Autowired
  private LngLatValidator lngLatValidator;

  @Override
  public boolean supports(final Class<?> clazz) {
    return Tenant.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    if (target instanceof Tenant) {

      final MapParams mapParams = ((Tenant) target).getMapParams();

      // Try to parse the zoom level as an positive integer between 0 and 21
      // See https://developers.google.com/maps/documentation/static-maps/intro#Zoomlevels
      if (mapParams.getZoomLevel() < MIN_ZOOM_LEVEL || mapParams.getZoomLevel() > MAX_ZOOM_LEVEL) {
        errors.rejectValue("mapParams.zoomLevel", "tenant.error.mapParams.zoomLevel.invalidValue");
      }

      // Validate map center coordinates
      final BindingResult lngLatErrors = new DataBinder(mapParams.getCenter()).getBindingResult();
      lngLatValidator.validate(mapParams.getCenter(), lngLatErrors);
      if (lngLatErrors.hasErrors()) {
        if (!lngLatErrors.getFieldErrors("latitude").isEmpty()) {
          errors.rejectValue("mapParams.center.latitude", lngLatErrors.getFieldErrors("latitude").get(0).getCode());
        }
        if (!lngLatErrors.getFieldErrors("longitude").isEmpty()) {
          errors.rejectValue("mapParams.center.longitude", lngLatErrors.getFieldErrors("longitude").get(0).getCode());
        }
      }
    }
  }
}
