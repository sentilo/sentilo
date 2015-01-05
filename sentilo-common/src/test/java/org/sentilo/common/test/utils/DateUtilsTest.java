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
package org.sentilo.common.test.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sentilo.common.utils.DateUtils;

public class DateUtilsTest {

  @Before
  public void setUp() {
    System.setProperty("user.timezone", "UTC");
  }

  @Test
  public void formatAndParseDate() {
    final Date currentDate = new Date();
    final String formatDate = DateUtils.toStringTimestamp(currentDate);
    assertNotNull(formatDate);
    final Date parseDate = DateUtils.stringToDate(formatDate);
    assertNotNull(parseDate);
    assertEquals(formatDate, DateUtils.toStringTimestamp(parseDate));
  }

  @Test
  public void nullStringToDate() {
    assertNull(DateUtils.stringToDate(null));
  }

  @Test(expected = IllegalArgumentException.class)
  public void invalidStringToDate() {
    DateUtils.stringToDate("12/12/2013");
  }

  @Test
  public void nullTimestampToString() {
    assertNull(DateUtils.timestampToString(null));
  }

  @Test
  public void timeZoneTimestampToDate() {
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10CET"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10CEST"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10GMT"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10UTC"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10+0200"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10-0200"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10GMT-02:00"));
    assertNotNull(DateUtils.stringToDate("12/12/12014T23:34:10AST"));
  }

  @Test
  public void timestampToString() {
    final Date currentDate = new Date();
    final Long currentTimestamp = currentDate.getTime();

    final String formatDate = DateUtils.timestampToString(currentTimestamp);
    assertNotNull(formatDate);
    assertEquals(formatDate, DateUtils.toStringTimestamp(currentDate));
  }

  @Test(expected = IllegalArgumentException.class)
  public void invalidTimestampToMillis() {
    DateUtils.toMillis("12/12/2013");
  }

  @Test
  public void toMillis() {
    final Date currentDate = new Date();
    final String formatDate = DateUtils.toStringTimestamp(currentDate);
    assertTrue(DateUtils.toMillis(formatDate) > 0);
  }

  @Test
  public void parseNullTimestamp() {
    assertNull(DateUtils.parseTimestamp(null));
  }

  @Test
  public void parseTimestamp() {
    final Date currentDate = new Date();
    final String formatDate = DateUtils.toStringTimestamp(currentDate);
    assertTrue(DateUtils.parseTimestamp(formatDate) > 0);
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseInvalidFormatTimestamp() {
    final String timestamp = "12/10/2013_23:12:23";
    DateUtils.parseTimestamp(timestamp);
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseTimestampWithoutTime() {
    final String timestamp = "12/10/2013";
    DateUtils.parseTimestamp(timestamp);
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseTransposeTimestamp() {
    final String timestamp = "11/23/2013T23:12:23";
    DateUtils.parseTimestamp(timestamp);
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseTranspose2Timestamp() {
    final String timestamp = "2013/11/23T23:12:23";
    DateUtils.parseTimestamp(timestamp);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toMillisTransposeTimestamp() {
    final String timestamp = "11/23/2013T23:12:23";
    DateUtils.toMillis(timestamp);
  }

  @Test
  public void parseTimestampWithoutTimeZone() {
    final String timestampWithTZ = "01/04/2014T10:23:34+0200";
    final String timestampWithoutTZ = "01/04/2014T08:23:34";

    Assert.assertEquals(DateUtils.parseTimestamp(timestampWithoutTZ), DateUtils.parseTimestamp(timestampWithTZ));
  }
}
