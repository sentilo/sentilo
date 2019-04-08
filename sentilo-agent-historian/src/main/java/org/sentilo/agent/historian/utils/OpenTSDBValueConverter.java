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
package org.sentilo.agent.historian.utils;

import java.text.ParseException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Converts values from org.sentilo.common.domain.MessageEvent to values accepted by OpenTSDB:
 * http://opentsdb.net/docs/build/html/user_guide/writing.html
 *
 * "integer" value bounds correspond to java.lang.Long extent. Floating point values correspond to
 * java.lang.Float.
 */
public class OpenTSDBValueConverter {

  private static final Logger LOGGER = LoggerFactory.getLogger(OpenTSDBValueConverter.class);

  private static Boolean metricsFromSensorType = true;

  private static final String ILLEGAL_CHARACTERS_REGEX = "[^a-zA-Z0-9-_\\/.]";

  /**
   * Return metric name either in form of &lt;event type&gt;.&lt;sensor type&gt; or &lt;event
   * type&gt;.&lt;component&gt;.&lt;sensor name&gt;, depending on property metrics.fromSensorType
   *
   * @param event
   * @return
   */
  public static String createMetricName(final EventMessage event) {

    String metric;

    if (metricsFromSensorType) {
      metric = event.getType().toLowerCase() + '.' + event.getSensorType();
    } else {
      metric = event.getType().toLowerCase() + '.' + event.getProvider() + '.' + event.getSensor();
    }

    return replaceIllegalCharacters(metric);
  }

  /**
   * OpenTSDB accepts only numeric and boolean values in Data Points.
   *
   * @param valueStr
   * @return Object of type Long or Float. Boolean values are converted to int 0 / 1
   */
  public static Object getSimpleValue(final String valueStr) throws ParseException {

    Object value = getLongValue(valueStr);
    if (value == null) {
      value = getFloatValue(valueStr);
    }
    if (value == null) {
      value = getBooleanValue(valueStr);
    }
    if (value == null) {
      LOGGER.debug("Could not parse value as number or boolean: " + valueStr);
      throw new ParseException("Only numeric or boolean values are allowed", 0);
    }

    return value;
  }

  public static boolean isComplexValue(final String value) {

    if (value == null) {
      return false;
    }

    return value.trim().startsWith("{") ? true : false;

  }

  /**
   * Searches recursively for numeric values in the JSON message and creates a map of metrics and
   * values. The metric's name is a contains property name for each level.
   *
   * @param parentMeasureName
   * @param value
   * @return
   */
  public static Map<String, Object> extractMeasuresFromComplexType(final String parentMeasureName, final String value) {

    try {
      final ObjectMapper mapper = new ObjectMapper();
      final JsonNode jsonNode = mapper.readTree(value);
      return extractMeasuresFromComplexType(parentMeasureName, jsonNode);

    } catch (final Exception e) {
      LOGGER.debug("Could not parse value as complexType : " + value);
      return new HashMap<String, Object>();
    }

  }

  /**
   * OpenTSDB accepts only characters included in a-z, A-Z, 0-9
   *
   * @param value
   * @return String without illegal characters
   */
  public static String replaceIllegalCharacters(final String value) {
    return StringUtils.hasText(value) ? value.replaceAll(ILLEGAL_CHARACTERS_REGEX, ".") : value;
  }

  private static Map<String, Object> extractMeasuresFromComplexType(final String parentMeasureName, final JsonNode jsonNode) {

    final Map<String, Object> unwrappedValues = new HashMap<String, Object>();

    if (jsonNode.isContainerNode()) {
      final Iterator<Entry<String, JsonNode>> i = jsonNode.fields();
      while (i.hasNext()) {
        final Entry<String, JsonNode> childEntry = i.next();
        unwrappedValues.putAll(extractMeasuresFromComplexType(parentMeasureName + "." + childEntry.getKey(), childEntry.getValue()));
      }
    } else {
      final String measureName = parentMeasureName;
      try {
        final Object value = getSimpleValue(jsonNode.asText());
        unwrappedValues.put(measureName, value);
      } catch (final ParseException pe) {
        // probably String or some non-numeric value. Pass
      }
    }
    return unwrappedValues;
  }

  private static Long getLongValue(final String value) {
    try {
      return Long.parseLong(value);
    } catch (final NumberFormatException e) {
      return null;
    }
  }

  private static Float getFloatValue(final String value) {
    try {
      final Float f = Float.parseFloat(value);
      if (f > -Float.MAX_VALUE && f < Float.MAX_VALUE) {
        return f;
      } else {
        return null;
      }
    } catch (final NumberFormatException e) {
      return null;
    }
  }

  private static Integer getBooleanValue(final String value) {

    if (value.equalsIgnoreCase(Boolean.TRUE.toString())) {
      return 1;
    } else if (value.equalsIgnoreCase(Boolean.FALSE.toString())) {
      return 0;
    } else {
      return null;
    }

  }

  public static Boolean getMetricsFromSensorType() {
    return metricsFromSensorType;
  }

  public static void setMetricsFromSensorType(final Boolean metricsFromSensorType) {
    LOGGER.info("SETTING metricsFromSensorType TO {}", metricsFromSensorType);
    OpenTSDBValueConverter.metricsFromSensorType = metricsFromSensorType;
  }

}
