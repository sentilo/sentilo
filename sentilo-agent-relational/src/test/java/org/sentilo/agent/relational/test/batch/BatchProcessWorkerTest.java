/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.relational.test.batch;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.relational.domain.Alarm;
import org.sentilo.agent.relational.domain.Data;
import org.sentilo.agent.relational.domain.Observation;
import org.sentilo.agent.relational.domain.Order;

public class BatchProcessWorkerTest {

  @Test
  public void subList() {
    final List<Data> dataToPersist = buildMockList();

    final List<Data> orders = subList(dataToPersist, Order.class);
    final List<Data> alarms = subList(dataToPersist, Alarm.class);
    final List<Data> observations = subList(dataToPersist, Observation.class);

    Assert.assertTrue(orders.size() == 15);
    Assert.assertTrue(observations.size() == 10);
    Assert.assertTrue(alarms.size() == 5);
  }

  private List<Data> buildMockList() {
    final List<Data> mockElements = new ArrayList<Data>();

    for (int i = 0; i < 10; i++) {
      mockElements.add(new Observation());
    }

    for (int i = 0; i < 5; i++) {
      mockElements.add(new Alarm());
    }

    for (int i = 0; i < 15; i++) {
      mockElements.add(new Order());
    }

    return mockElements;
  }

  private List<Data> subList(final List<Data> elements, final Class<? extends Data> filterType) {
    final List<Data> result = new ArrayList<Data>();
    for (final Data data : elements) {

      if (filterType.isAssignableFrom(data.getClass())) {
        result.add(data);
      }
    }
    return result;
  }
}
