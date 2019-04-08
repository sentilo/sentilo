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

import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.agent.relational.domain.Data;
import org.sentilo.agent.relational.repository.batch.BatchProcessCallback;
import org.sentilo.agent.relational.repository.batch.BatchProcessContext;
import org.sentilo.common.test.AbstractBaseTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;

public class BatchProcessContextTest extends AbstractBaseTest {

  @Mock
  private List<Data> dataToPersist;
  @Mock
  private JdbcTemplate jdbcTemplate;
  @Mock
  private PlatformTransactionManager transactionManager;
  @Mock
  private BatchProcessCallback callback;

  private Integer numMaxRetries = Integer.MAX_VALUE;
  private String tablesPrefix = "SENTILO";
  private String targetDs = "sentiloDs";

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void newContext() {
    final BatchProcessContext context =
        new BatchProcessContext(dataToPersist, jdbcTemplate, transactionManager, numMaxRetries, tablesPrefix, targetDs, callback);

    Assert.assertEquals(dataToPersist, context.getDataToPersist());
    Assert.assertEquals(jdbcTemplate, context.getJdbcTemplate());
    Assert.assertEquals(transactionManager, context.getTransactionManager());
    Assert.assertEquals((long) numMaxRetries, (long) context.getNumMaxRetries());
    Assert.assertEquals(tablesPrefix, context.getTablesPrefix());
    Assert.assertEquals(callback, context.getCallback());
    Assert.assertEquals(targetDs, context.getTargetDs());
  }
}
