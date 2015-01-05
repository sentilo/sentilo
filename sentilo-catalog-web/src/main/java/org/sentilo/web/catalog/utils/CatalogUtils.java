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
package org.sentilo.web.catalog.utils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.springframework.util.StringUtils;

public abstract class CatalogUtils extends SentiloUtils {

  public static final String ESCAPE_REGEXP_CHARACTER = "\\";
  public static final String ISO_8859_1 = "ISO-8859-1";

  public static String decodeAjaxParam(final String source) {
    String target = source;
    try {
      target = (source == null ? source : URLDecoder.decode(source, ISO_8859_1));
    } catch (final UnsupportedEncodingException ex) {
      // ignore
    }

    return target;
  }

  public static Location convertStringLocation(final String stringLocation) {
    // Coordinates has the format
    // "latitude1 longitude1, latitude2 longitude2, ....., latitudeN longitudeN"
    if (!StringUtils.hasText(stringLocation)) {
      return null;
    }

    final String[] coordinatesList = stringLocation.split(Constants.LOCATION_TOKEN_SPLITTER);
    final LngLat[] lngLatCoordinates = new LngLat[coordinatesList.length];

    int i = 0;
    for (final String coordinates : coordinatesList) {
      final int pos = coordinates.indexOf(Constants.LOCATION_TOKEN_DIVIDER);
      if (pos != -1) {
        final Double latitude = Double.parseDouble(coordinates.substring(0, pos).trim());
        final Double longitude = Double.parseDouble(coordinates.substring(pos + 1).trim());
        lngLatCoordinates[i++] = new LngLat(longitude, latitude);
      }
    }

    return new Location(lngLatCoordinates);
  }

  public static String locationToString(final Location location) {
    return (location == null ? null : location.toString());
  }

  public static String escapeRegexCharacter(final String character) {
    return ESCAPE_REGEXP_CHARACTER.concat(character);
  }

  private CatalogUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
