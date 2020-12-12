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
package org.sentilo.web.catalog.editor;

import java.beans.PropertyEditorSupport;

import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.utils.CatalogUtils;

public class LocationPropertyEditor extends PropertyEditorSupport {

  @Override
  public void setAsText(final String text) {
    final Location location = CatalogUtils.convertStringLocation(parseCoordinates(text));
    setValue(location);
  }

  public String parseCoordinates(final String clientCoordinates) {
    // Coordinates from the client side is an array with the format:
    // [(lat1,lng1),(lat2,lng2), ... ,(latN,lngN)]
    // but server side expects that coordinates match the following format (format fixed by the API
    // Rest):
    // [lat1 lng1,lat2 lng2, ... ,latN lngN]

    final String[] result = clientCoordinates.split("\\),\\(");
    final StringBuilder coordinatesFormatted = new StringBuilder();
    boolean first = true;

    for (final String latLngToken : result) {
      if (!first) {
        coordinatesFormatted.append(SentiloConstants.LOCATION_TOKEN_SPLITTER);
      }

      coordinatesFormatted.append(latLngToken.replace("(", "").replace(")", "").replace(",", SentiloConstants.LOCATION_TOKEN_DIVIDER));
      first = false;
    }

    return coordinatesFormatted.toString();
  }

}
