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
package org.sentilo.agent.relational.test.repository;

import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.relational.repository.batch.BatchProcessResult;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.test.AbstractBaseTest;

public class BatchProcessResultTest extends AbstractBaseTest {

  private final List<EventMessage> elementsToPersist = Collections.<EventMessage>emptyList();
  private final int numElementsPersisted = Integer.MAX_VALUE;

  @Test
  public void newContext() {
    final BatchProcessResult result = new BatchProcessResult(elementsToPersist, numElementsPersisted);

    Assert.assertEquals(elementsToPersist, result.getElementsToPersist());
    Assert.assertEquals(0, result.getNumElementsToPersist());
    Assert.assertEquals(numElementsPersisted, result.getNumElementsPersisted());

  }
}
