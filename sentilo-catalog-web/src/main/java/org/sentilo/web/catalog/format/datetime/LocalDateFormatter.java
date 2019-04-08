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
package org.sentilo.web.catalog.format.datetime;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.sentilo.common.utils.DateUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.UserConfigContext;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.format.datetime.DateFormatter;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class LocalDateFormatter extends DateFormatter {

  private static final String DEFAULT_DATE_PATTERN = SentiloConstants.TIMESTAMP_PATTERN;

  private String lastDatePattern;

  private TimeZone lastTimeZone;

  public String parseUtcTimeToLocalTime(final String utcTimestamp) {
    setPattern(DEFAULT_DATE_PATTERN);
    return StringUtils.hasText(utcTimestamp) ? print(DateUtils.stringToDate(utcTimestamp), LocaleContextHolder.getLocale()) : utcTimestamp;
  }

  public String printCurrentAsLocalTime() {
    return printAsLocalTime(System.currentTimeMillis());
  }

  public String printAsLocalTime(final Long timestamp) {
    return printAsLocalTime(new Date(timestamp));
  }

  public String printAsLocalTime(final Date date) {
    return printAsLocalTime(date, DEFAULT_DATE_PATTERN);
  }

  public String printAsLocalTime(final Long timestamp, final String pattern) {
    return printAsLocalTime(new Date(timestamp), pattern);
  }

  public String printAsLocalTime(final Date date, final String pattern) {
    setPattern(pattern);
    return date != null ? print(date, LocaleContextHolder.getLocale()) : "";
  }

  public long parseLocalTime(final String localTimestamp) {
    try {
      setPattern(DEFAULT_DATE_PATTERN);
      return parse(localTimestamp, LocaleContextHolder.getLocale()).getTime();
    } catch (final ParseException e) {
      throw new IllegalArgumentException("Error parsing date " + localTimestamp, e);
    }
  }

  public String print(final Date date, final Locale locale) {
    return super.print(date, locale);
  }

  @Override
  protected DateFormat getDateFormat(final Locale locale) {
    // Get user config to apply formats
    final UserConfigContext context = UserConfigContextHolder.getContext();
    if (context != null) {
      // Restore original values
      setPattern(context.getUserDatePattern());
      setTimeZone(context.getUserTimeZone());
    }

    // Get date format
    final DateFormat dateFormat = super.getDateFormat(locale);

    // Restore original values
    setPattern(lastDatePattern);
    setTimeZone(lastTimeZone);

    return dateFormat;
  }

  @Override
  public void setPattern(final String pattern) {
    lastDatePattern = pattern;
    super.setPattern(pattern);
  }

  @Override
  public void setTimeZone(final TimeZone timeZone) {
    lastTimeZone = timeZone;
    super.setTimeZone(timeZone);
  }

}
