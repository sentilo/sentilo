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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import org.springframework.util.StringUtils;

public abstract class DateUtils {

  private static final String TIMESTAMP_PATTERN = SentiloConstants.TIMESTAMP_PATTERN;
  private static final String TIMEZONE_TIMESTAMP_PATTERN = "dd/MM/yyyy'T'HH:mm:ssZ";
  private static final String UTC_SUFFIX = "+0000";
  // DateFormat used to generate strings, in the UTC base, from internal timestamps
  private static final DateFormat PSAB_DF;
  // DateFormat used to parse Dates/Long from incoming timestamps (which may contains a time
  // zone)
  private static final DateFormat TZ_PSAB_DF;

  private static final Lock LOCK_PSAB_DF = new ReentrantLock();
  private static final Lock LOCK_TZ_PSAB_DF = new ReentrantLock();

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
    LOCK_PSAB_DF.lock();
    try {
      return PSAB_DF.format(date);
    } finally {
      LOCK_PSAB_DF.unlock();
    }
  }

  public static String timestampToString(final Long timestamp) {
    LOCK_PSAB_DF.lock();
    try {
      return timestamp == null ? null : PSAB_DF.format(timestamp);
    } finally {
      LOCK_PSAB_DF.unlock();
    }
  }

  public static long toMillis(final String timestamp) {
    return stringToDate(timestamp).getTime();
  }

  public static Date stringToDate(final String date) {
    LOCK_TZ_PSAB_DF.lock();
    try {
      // First, validate that localTime has TZ defined. If not defined, we will treat it as a UTC
      // date (+0000)
      return StringUtils.hasText(date) ? TZ_PSAB_DF.parse(formatToUTC(date)) : null;
    } catch (final ParseException e) {
      throw new IllegalArgumentException("Error parsing date " + date, e);
    } finally {
      LOCK_TZ_PSAB_DF.unlock();
    }
  }

  public static Long parseTimestamp(final String timestamp) {
    final Date date = stringToDate(timestamp);
    return date != null ? date.getTime() : null;
  }

  public static boolean sameDay(final long ts1, final long ts2) {
    final Calendar cal1 = Calendar.getInstance();
    cal1.setTimeInMillis(ts1);
    final Calendar cal2 = Calendar.getInstance();
    cal2.setTimeInMillis(ts2);

    return cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR);
  }

  private static String formatToUTC(final String localTime) {
    // If localTime has the timeZone filled in, returns localTime unmodified. Otherwise, returns
    // localTime + '+0000'
    // To check if timeZone is filled in, we try to parse localTime with the TZ_PSAB_DF format
    try {
      TZ_PSAB_DF.parse(localTime);
      return localTime;
    } catch (final ParseException e) {
      return localTime.concat(UTC_SUFFIX);
    }
  }

}
