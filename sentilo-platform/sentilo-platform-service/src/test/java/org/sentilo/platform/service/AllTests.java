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
package org.sentilo.platform.service;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.sentilo.platform.service.test.dao.JedisKeysBuilderTest;
import org.sentilo.platform.service.test.dao.JedisPoolUtilsTest;
import org.sentilo.platform.service.test.dao.JedisSequenceUtilsTest;
import org.sentilo.platform.service.test.dao.JedisTemplateTest;
import org.sentilo.platform.service.test.listener.ConnectionMonitorListenerTest;
import org.sentilo.platform.service.test.listener.MonitorMessageListenerImplTest;
import org.sentilo.platform.service.test.listener.NotificationSenderTest;
import org.sentilo.platform.service.test.parser.CatalogServiceParserTest;
import org.sentilo.platform.service.test.service.AdminServiceImplTest;
import org.sentilo.platform.service.test.service.AlarmServiceImplTest;
import org.sentilo.platform.service.test.service.DataServiceImplTest;
import org.sentilo.platform.service.test.service.OrderServiceImplTest;
import org.sentilo.platform.service.test.service.ResourceServiceImplTest;
import org.sentilo.platform.service.test.service.SubscribeServiceImplTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({AlarmServiceImplTest.class, DataServiceImplTest.class, OrderServiceImplTest.class, SubscribeServiceImplTest.class,
    CatalogServiceParserTest.class, AdminServiceImplTest.class, ResourceServiceImplTest.class, ConnectionMonitorListenerTest.class,
    MonitorMessageListenerImplTest.class, NotificationSenderTest.class, JedisKeysBuilderTest.class, JedisPoolUtilsTest.class,
    JedisSequenceUtilsTest.class, JedisTemplateTest.class})
public class AllTests {

}
