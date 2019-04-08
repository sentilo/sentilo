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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.web.catalog.utils.AlarmMessageComparator;
import org.sentilo.web.catalog.utils.OrderMessageComparator;

public class MessageComparatorsTest {

  @Mock
  private AlarmMessage alarmMessage1;

  @Mock
  private AlarmMessage alarmMessage2;

  @Mock
  private OrderMessage orderMessage1;

  @Mock
  private OrderMessage orderMessage2;

  @Spy
  private AlarmMessageComparator alarmMessageComparator;

  @Spy
  private OrderMessageComparator orderMessageComparator;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void compareAlarmMessages() {
    when(alarmMessage1.getTimestampToMillis()).thenReturn(1l);
    when(alarmMessage2.getTimestampToMillis()).thenReturn(2l);

    final int result = alarmMessageComparator.compare(alarmMessage1, alarmMessage2);

    Assert.assertTrue(result < 0);
  }

  @Test
  public void compareOrderMessages() {
    when(orderMessage1.getTimestampToMillis()).thenReturn(1l);
    when(orderMessage2.getTimestampToMillis()).thenReturn(2l);

    final int result = orderMessageComparator.compare(orderMessage1, orderMessage2);

    Assert.assertTrue(result < 0);
  }

}
