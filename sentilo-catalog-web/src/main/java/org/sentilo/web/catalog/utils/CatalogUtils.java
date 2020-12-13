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
package org.sentilo.web.catalog.utils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.AlphabeticalSortable;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.dto.OptionListDTOFactory;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

public abstract class CatalogUtils extends SentiloUtils {

  public static final String ESCAPE_REGEXP_CHARACTER = "\\";
  public static final String ISO_8859_1 = "ISO-8859-1";

  protected CatalogUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String decodeAjaxParam(final String source) {
    String target = source;
    try {
      target = source == null ? source : URLDecoder.decode(source, ISO_8859_1);
    } catch (final UnsupportedEncodingException ex) {
      // ignore
    }

    return target;
  }

  public static Location convertStringLocation(final String stringLocation) {
    // Coordinates has the format
    // "latitude1 longitude1, latitude2 longitude2, ....., latitudeN longitudeN"
    if (!stringIsNotEmptyOrNull(stringLocation)) {
      return null;
    }

    final String[] coordinatesList = stringLocation.split(SentiloConstants.LOCATION_TOKEN_SPLITTER);
    final LngLat[] lngLatCoordinates = new LngLat[coordinatesList.length];

    int i = 0;
    for (final String coordinates : coordinatesList) {
      final int pos = coordinates.indexOf(SentiloConstants.LOCATION_TOKEN_DIVIDER);
      if (pos != -1) {
        final Double latitude = Double.parseDouble(coordinates.substring(0, pos).trim());
        final Double longitude = Double.parseDouble(coordinates.substring(pos + 1).trim());
        lngLatCoordinates[i++] = new LngLat(longitude, latitude);
      }
    }

    return new Location(lngLatCoordinates);
  }

  public static String locationToString(final Location location) {
    return location == null ? null : location.toString();
  }

  public static String escapeRegexCharacter(final String character) {
    return ESCAPE_REGEXP_CHARACTER.concat(character);
  }

  public static List<String> tagsToStringList(final String tags) {
    return tags != null ? Arrays.asList(tags.split("\\s*,\\s*")) : null;
  }

  public static Class<? extends CatalogDocument> getCollectionType(final Collection<? extends CatalogDocument> collection) {
    Assert.notEmpty(collection, "collection must not be empty to get its elements type");
    return collection.iterator().next().getClass();
  }

  public static long getMaxSystemTimeMillis() {
    return new Date(Long.MAX_VALUE).getTime();
  }

  public static boolean isJSONValid(final String value) {
    final ObjectMapper mapper = new ObjectMapper();
    try {
      // Try to parse a possible JSON object
      mapper.readTree(value);
      return true;
    } catch (final Exception e) {
      // Do nothing, value is not a JSON
      return false;
    }
  }

  public static <T> String mapToString(final Map<String, T> keyValues) {
    final StringBuilder sb = new StringBuilder();
    if (!CollectionUtils.isEmpty(keyValues)) {
      for (final Map.Entry<String, T> entry : keyValues.entrySet()) {
        if (sb.length() > 0) {
          sb.append("; ");
        }
        sb.append(entry.getKey() + "=" + entry.getValue());
      }
    }
    return sb.toString();
  }

  public static <T> String collectionToString(final Collection<T> values) {
    final StringBuilder sb = new StringBuilder();
    if (!CollectionUtils.isEmpty(values)) {
      for (final Object entry : values) {
        if (sb.length() > 0) {
          sb.append(", ");
        }
        sb.append(entry);
      }
    }
    return sb.toString();
  }

  public static <T> String arrayToString(final T[] values) {
    final StringBuilder sb = new StringBuilder();
    if (!arrayIsEmpty(values)) {
      sb.append(collectionToString(Arrays.asList(values)));
    }

    return sb.toString();
  }

  public static String[] enumGetNames(final Class<? extends Enum<?>> enumClass) {
    return Arrays.toString(enumClass.getEnumConstants()).replaceAll("^.|.$", "").split(", ");
  }

  public static List<OptionDTO> toOptionList(final String suffixes, final String prefix, final MessageSource messageSource) {
    final String[] aSuffixes = suffixes.split(SentiloConstants.COMMA_TOKEN_SPLITTER);
    return toOptionList(aSuffixes, prefix, messageSource);
  }

  public static List<OptionDTO> toOptionList(final Class<? extends Enum<?>> enumClass, final String prefix, final MessageSource messageSource) {
    final String[] names = Arrays.toString(enumClass.getEnumConstants()).replaceAll("^.|.$", "").split(", ");
    return toOptionList(names, prefix, messageSource);
  }

  public static List<OptionDTO> toOptionList(final String[] suffixes, final String prefix, final MessageSource messageSource) {
    final List<OptionDTO> optionsList = OptionListDTOFactory.build(suffixes, prefix, messageSource);
    return sortAlphabetically(optionsList);
  }

  public static <T extends AlphabeticalSortable> List<OptionDTO> toOptionList(final List<T> source) {
    return sortAlphabetically(OptionListDTOFactory.build(source));
  }

  public static <T extends AlphabeticalSortable> List<T> sortAlphabetically(final List<T> list) {
    Collator.getInstance(LocaleContextHolder.getLocale());
    final List<T> mutableList = new ArrayList<T>(list);
    Collections.sort(mutableList, new AlphabeticalComparator());
    return mutableList;
  }

  public static boolean isStaticRequest(final HttpServletRequest request) {
    final String requestUri = request.getRequestURI();
    final String requestContextPath = request.getContextPath();

    final String mappingPath = requestUri.substring(requestContextPath.length());
    final String[] tokens = mappingPath.trim().split("/");

    return tokens.length > 0 && "static".equals(tokens[1]);
  }
}
