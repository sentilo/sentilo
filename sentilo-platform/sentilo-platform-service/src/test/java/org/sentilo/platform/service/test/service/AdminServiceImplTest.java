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
package org.sentilo.platform.service.test.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.impl.AdminServiceImpl;


public class AdminServiceImplTest {
	 			
	@Mock private JedisSequenceUtils jedisSequenceUtils;
	
	private AdminServiceImpl service;
	
	@Before
	public void setUp() {		
		MockitoAnnotations.initMocks(this);
		service = new AdminServiceImpl();				
		service.setJedisSequenceUtils(jedisSequenceUtils);
		
	}

	@Test
	public void getStatistics(){
		long alarms = 2;
		long orders = 10;
		long observations = 120;
		
		long events = alarms+orders+observations;
		
		when(jedisSequenceUtils.getCurrentAmid()).thenReturn(alarms);
		when(jedisSequenceUtils.getCurrentSoid()).thenReturn(orders);
		when(jedisSequenceUtils.getCurrentSdid()).thenReturn(observations);
		
		Statistics stats = service.getStatistics();
				
		verify(jedisSequenceUtils).getCurrentAmid();				
		verify(jedisSequenceUtils).getCurrentSdid();
		verify(jedisSequenceUtils).getCurrentSoid();
		
		assertEquals(new Long(events), stats.getEvents().getTotal());
		assertEquals(new Long(alarms), stats.getEvents().getAlarms());
		assertEquals(new Long(orders), stats.getEvents().getOrders());
		assertEquals(new Long(observations), stats.getEvents().getObservations());
		
	}		
}
