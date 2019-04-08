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
package org.sentilo.web.catalog.test.format.datetime;

import java.util.Date;
import java.util.TimeZone;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.UserConfigContextHolder;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;

public class LocalDateFormatterTest {

  private LocalDateFormatter formatter = new LocalDateFormatter();

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    UserConfigContextHolder.clearContext();
    formatter.setTimeZone(TimeZone.getTimeZone("CET"));
  }

  @Test
  public void parseUtcTimeToLocalTime() {
    final String utcTimestamp = "22/10/2014T05:56:43";

    final String localTimestamp = formatter.parseUtcTimeToLocalTime(utcTimestamp);

    Assert.assertEquals("22/10/2014T07:56:43", localTimestamp);
  }

  @Test
  public void printCurrentAsLocalTime() {

    final String localCurrentTimestamp = formatter.printCurrentAsLocalTime();

    Assert.assertNotNull(localCurrentTimestamp);
    Assert.assertTrue(localCurrentTimestamp.length() == 19);
  }

  @Test
  public void printAsLocalTime() {
    final Date currentDate = new Date();

    final String localTs1 = formatter.printAsLocalTime(currentDate);
    final String localTs2 = formatter.printAsLocalTime(currentDate.getTime());
    final String localTs3 = formatter.printAsLocalTime(currentDate, SentiloConstants.TIMESTAMP_PATTERN);
    final String localTs4 = formatter.printAsLocalTime(currentDate.getTime(), SentiloConstants.TIMESTAMP_PATTERN);

    Assert.assertEquals(localTs1, localTs2);
    Assert.assertEquals(localTs3, localTs1);
    Assert.assertEquals(localTs4, localTs3);
  }

  @Test
  public void parseLocalTime() {
    final String locaTimestamp = "22/10/2014T07:56:43";
    final long expected = 1413957403000l;

    final long result = formatter.parseLocalTime(locaTimestamp);

    Assert.assertEquals(expected, result);
  }

}
