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
package org.sentilo.web.catalog.test.context;

import java.util.TimeZone;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.context.UserConfigContextImpl;
import org.sentilo.web.catalog.utils.Constants;

public class UserConfigContextImplTest {

  @Test
  public void defaultConfig() {
    final UserConfigContextImpl config = new UserConfigContextImpl();

    Assert.assertEquals(SentiloConstants.TIMESTAMP_PATTERN, config.getUserDatePattern());
    Assert.assertEquals(TimeZone.getTimeZone(Constants.DEFAULT_TIME_ZONE), config.getUserTimeZone());
    Assert.assertEquals(Constants.DEFAULT_CHART_POINTS_NUMBER, config.getChartVisiblePointsNumber());
  }

  @Test
  public void customConfig() {
    final Integer userChartPointsNum = 10;
    final String datePattern = "yyyy/MM/dd HH:mm";
    final TimeZone tz = TimeZone.getTimeZone("GMT+02:00");

    final UserConfigContextImpl config = new UserConfigContextImpl(tz, datePattern, userChartPointsNum);

    Assert.assertEquals(datePattern, config.getUserDatePattern());
    Assert.assertEquals(tz, config.getUserTimeZone());
    Assert.assertEquals(userChartPointsNum, config.getChartVisiblePointsNumber());
  }

  @Test
  public void defaultConfigToString() {
    final UserConfigContextImpl config = new UserConfigContextImpl();
    final String sConfig = config.toString();

    final String expected =
        "org.sentilo.web.catalog.context.UserConfigContextImpl [ userDatePattern: dd/MM/yyyy'T'HH:mm:ss|userTimeZone: UTC|userChartNumObs: 10 ]";

    Assert.assertEquals(expected, sConfig);
  }

}
