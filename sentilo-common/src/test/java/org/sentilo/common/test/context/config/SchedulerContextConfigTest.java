/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.test.context.config;

import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.bson.Document;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.context.config.scheduler.SchedulerContextConfig;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.mongo.MongoLockProvider;

public class SchedulerContextConfigTest {

  @Mock
  private MongoClient mongoClient;

  @Mock
  private Properties sentiloConfigProperties;

  @Mock
  private MongoDatabase db;

  @Mock
  private MongoCollection<Document> dbCollection;

  @InjectMocks
  private SchedulerContextConfig contextConfig;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(sentiloConfigProperties.get("sentilo.mongodb.database")).thenReturn("sentilo");
    when(mongoClient.getDatabase(anyString())).thenReturn(db);
    when(db.getCollection(anyString())).thenReturn(dbCollection);
  }

  @Test
  public void testLockProvider() {
    final LockProvider lockProvider = contextConfig.lockProvider(mongoClient);
    assertTrue(lockProvider instanceof MongoLockProvider);
  }

}
