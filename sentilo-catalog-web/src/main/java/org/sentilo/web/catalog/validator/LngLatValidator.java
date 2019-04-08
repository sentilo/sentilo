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

import org.sentilo.web.catalog.domain.LngLat;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class LngLatValidator implements Validator {

  private static final double MIN_LAT = -90.0;

  private static final double MAX_LAT = 90.0;

  private static final double MIN_LNG = -180;

  private static final double MAX_LNG = 180;

  @Override
  public boolean supports(final Class<?> clazz) {
    return LngLat.class.equals(clazz);
  }

  @Override
  public void validate(final Object target, final Errors errors) {
    if (target instanceof LngLat) {
      final LngLat lngLat = (LngLat) target;

      if (lngLat.getLatitude() != null && lngLat.getLongitude() != null) {
        if (lngLat.getLatitude() < MIN_LAT || lngLat.getLatitude() > MAX_LAT) {
          errors.rejectValue("latitude", "location.error.latitude");
        }
        if (lngLat.getLongitude() < MIN_LNG || lngLat.getLongitude() > MAX_LNG) {
          errors.rejectValue("longitude", "location.error.longitude");
        }
      } else if (lngLat.getLatitude() != null && lngLat.getLongitude() == null || lngLat.getLatitude() == null && lngLat.getLongitude() != null) {
        errors.rejectValue("latitude", "location.error.blankPoint");
        errors.rejectValue("longitude", "location.error.blankPoint");
      }
    }
  }
}
