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
package org.sentilo.common.utils;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;

public abstract class SentiloUtils {

  protected static final Logger LOGGER = LoggerFactory.getLogger(SentiloUtils.class);
  private static final String hostname = initHostName();

  protected SentiloUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String getHostName() {
    return hostname;
  }

  private static String initHostName() {
    String hostname = "UNKNOWN";
    try {
      hostname = InetAddress.getLocalHost().getHostName();
    } catch (final UnknownHostException uhe) {
      LOGGER.warn("Local hostname could not be resolved. UNKNOWN word will be used");
    }

    return hostname;
  }

  @Nullable
  public static <T> T getElementAt(final T[] a, final int pos) {
    return (arrayIsEmpty(a) || a.length <= pos) ? null : a[pos];
  }

  public static boolean isArrayOrCollection(final Object value) {
    return value == null ? false : Collection.class.isAssignableFrom(value.getClass()) || value.getClass().isArray();
  }

  public static boolean arrayIsEmpty(final Object[] source) {
    return source == null || source.length == 0;
  }

  public static boolean stringIsNotEmptyOrNull(final String value) {
    return StringUtils.hasText(value) && !"null".equalsIgnoreCase(value);
  }

  public static String buildNewInternalErrorCode(final String prefix) {
    // Internal error codes have the format: prefix-timestamp
    return prefix + "-" + System.currentTimeMillis();
  }

  public static boolean arrayContainsValue(final String[] list, final String value) {
    final List<String> valuesList = Arrays.asList(list);
    return valuesList.contains(value);
  }

  public static boolean areEquals(final String value1, final String value2) {
    if (value1 == null && value2 == null) {
      return true;
    } else if (value1 == null && value2 != null) {
      return false;
    } else {
      return value1.equals(value2);
    }
  }

  /**
   * Add the specified values to the beginning of the list.
   *
   * @param baseList
   * @param values
   */
  public static List<String> addValuesToBeginningList(final List<String> baseList, final String... values) {
    final List<String> result = new ArrayList<String>();

    if (!arrayIsEmpty(values)) {
      final List<String> valuesList = Arrays.asList(values);
      result.addAll(valuesList);
    }
    result.addAll(baseList);
    return result;
  }

  public static boolean isDouble(final String value) {
    boolean isDouble = true;
    try {
      Double.parseDouble(value);
    } catch (final NumberFormatException nfe) {
      isDouble = false;
    }

    return isDouble;
  }

  public static boolean isValidLocationFormat(final String location) {
    boolean valid = true;
    if (SentiloUtils.stringIsNotEmptyOrNull(location)) {
      final String[] coordinatesList = location.split(SentiloConstants.LOCATION_TOKEN_SPLITTER);
      for (int i = 0; i < coordinatesList.length && valid; i++) {
        valid = valid & validateCoordinatesFormat(coordinatesList[i]);
      }
    }

    return valid;
  }

  private static boolean validateCoordinatesFormat(final String coordinates) {
    boolean areValidCoordinates = false;
    final String coordinatesTrimmed = coordinates.trim();
    final String[] coordinatesTokens = coordinatesTrimmed.split(SentiloConstants.LOCATION_TOKEN_DIVIDER);
    if (coordinatesTokens.length == 2 && isValidLatLng(coordinatesTokens)) {
      areValidCoordinates = true;
    }

    return areValidCoordinates;
  }

  private static boolean isValidLatLng(final String[] coordinatesTokens) {
    return isValidLatitude(coordinatesTokens[0].trim()) && isValidLongitude(coordinatesTokens[1].trim());
  }

  private static boolean isValidLatitude(final String latitude) {
    return isDouble(latitude) && isValidLatitude(Double.parseDouble(latitude));
  }

  private static boolean isValidLongitude(final String longitude) {
    return isDouble(longitude) && isValidLongitude(Double.parseDouble(longitude));
  }

  public static boolean isValidLatitude(final Double latitude) {
    return isDoubleWithinRange(latitude, SentiloConstants.MIN_LAT, SentiloConstants.MAX_LAT);
  }

  public static boolean isValidLongitude(final Double longitude) {
    return isDoubleWithinRange(longitude, SentiloConstants.MIN_LNG, SentiloConstants.MAX_LNG);
  }

  private static boolean isDoubleWithinRange(final double value, final double min, final double max) {
    return value >= min && value <= max;
  }
}
