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
package org.sentilo.platform.server.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;


public class HandlerLocatorTest {

	private HandlerLocator locator;
	@Mock private AbstractHandler dummyCatalogService;
	@Mock private AbstractHandler dummyDataService;
	@Mock private AbstractHandler dummySubscribeService;
	@Mock private AbstractHandler dummyAlarmService;
	@Mock private AbstractHandler dummyOrderService;

	@Before
	public void setUp() {
		MockitoAnnotations.initMocks(this);		
		locator = new HandlerLocator();
		locator.add("/catalog", dummyCatalogService);
		locator.add("/data", dummyDataService);
		locator.add("/order", dummyOrderService);
		locator.add("/alarm", dummyAlarmService);
		locator.add("/subscribe", dummySubscribeService);
	}

	@Test
	public void lookForService() {
		AbstractHandler result = locator.lookup("/catalog");
		assertNotNull(result);
		assertEquals(dummyCatalogService, result);
		
		result = locator.lookup("/data");
		assertNotNull(result);
		assertEquals(dummyDataService, result);
		
		result = locator.lookup("/alarm");
		assertNotNull(result);
		assertEquals(dummyAlarmService, result);
		
		result = locator.lookup("/order");
		assertNotNull(result);
		assertEquals(dummyOrderService, result);
		
		result = locator.lookup("/subscribe");
		assertNotNull(result);
		assertEquals(dummySubscribeService, result);
	}
	
	@Test()
	public void lookForUnexistingService() {
		AbstractHandler result = locator.lookup("unexisting");
		assertNull(result);
	}	
}
