/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
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
package org.sentilo.platform.server;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.sentilo.platform.server.handler.HandlerLocatorTest;
import org.sentilo.platform.server.handler.impl.AdminHandlerTest;
import org.sentilo.platform.server.handler.impl.AlarmHandlerTest;
import org.sentilo.platform.server.handler.impl.CatalogHandlerTest;
import org.sentilo.platform.server.handler.impl.DataHandlerTest;
import org.sentilo.platform.server.handler.impl.OrderHandlerTest;
import org.sentilo.platform.server.handler.impl.SubscribeHandlerTest;
import org.sentilo.platform.server.parser.AdminParserTest;
import org.sentilo.platform.server.parser.AlarmParserTest;
import org.sentilo.platform.server.parser.CatalogParserTest;
import org.sentilo.platform.server.parser.DataParserTest;
import org.sentilo.platform.server.parser.OrderParserTest;
import org.sentilo.platform.server.parser.SubscribeParserTest;
import org.sentilo.platform.server.request.SentiloResourceTests;


@RunWith(Suite.class)

@Suite.SuiteClasses({ 
  SentiloResourceTests.class,
  AlarmParserTest.class,
  OrderParserTest.class,
  DataParserTest.class,
  SubscribeParserTest.class,
  CatalogParserTest.class,
  AdminParserTest.class,
  HandlerLocatorTest.class,
  AlarmHandlerTest.class,
  OrderHandlerTest.class,
  SubscribeHandlerTest.class,
  DataHandlerTest.class,
  CatalogHandlerTest.class,
  AdminHandlerTest.class}
)
public class AllTests {

}
