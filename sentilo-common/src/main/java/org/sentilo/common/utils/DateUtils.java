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
package org.sentilo.common.utils;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.springframework.util.StringUtils;

public abstract class DateUtils {

  private static final String TIMESTAMP_PATTERN = "dd/MM/yyyy'T'HH:mm:ss";
  private static final String TIMEZONE_TIMESTAMP_PATTERN = "dd/MM/yyyy'T'HH:mm:ssZ";
  private static final String UTC_SUFFIX = "+0000";
  // DateFormat to use to format output timestamps
  private static final DateFormat PSAB_DF;
  // DateFormat to use to parser incoming timestamps
  private static final DateFormat TZ_PSAB_DF;

  static {
    PSAB_DF = new SimpleDateFormat(TIMESTAMP_PATTERN);
    PSAB_DF.setLenient(false);
    TZ_PSAB_DF = new SimpleDateFormat(TIMEZONE_TIMESTAMP_PATTERN);
    TZ_PSAB_DF.setLenient(false);
  }

  private DateUtils() {
    throw new AssertionError();
  }

  public static String toStringTimestamp(final Date date) {
    return PSAB_DF.format(date);
  }

  public static String timestampToString(final Long timestamp) {
    return (timestamp == null ? null : PSAB_DF.format(timestamp));
  }

  public static long toMillis(final String timestamp) {
    return stringToDate(timestamp).getTime();
  }

  public static Date stringToDate(final String date) {
    try {
      // First, validate that localTime has TZ defined. If not defined, we will treat it as a UTC
      // date (+0000)
      return (StringUtils.hasText(date) ? TZ_PSAB_DF.parse(formatToUTC(date)) : null);
    } catch (final ParseException e) {
      throw new IllegalArgumentException("Error parsing date", e);
    }
  }

  public static Long parseTimestamp(final String timestamp) {
    final Date date = stringToDate(timestamp);
    return (date != null ? date.getTime() : null);
  }

  private static String formatToUTC(final String localTime) {
    // Returns localTime + '+0000' . There is no problem if timeZone token is duplicate because
    // pattern only inspects the first occurrence of the timeZone in localTime
    return localTime.concat(UTC_SUFFIX);
  }

}
