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

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.platform.service.impl.CatalogServiceImpl;


public class CatalogServiceImplTest {
	 
	@Mock private CatalogInputMessage message;	
	@Mock private RESTClient restClient;
	
	private CatalogServiceImpl service;
	
	@Before
	public void setUp() {		
		MockitoAnnotations.initMocks(this);
		service = new CatalogServiceImpl();				
		service.setRestClient(restClient);
		
	}

	@Test
	public void insertSensors(){
		when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
		when(message.getProviderId()).thenReturn("provider1");
		when(message.getBody()).thenReturn("");
		String path = "api/provider/"+message.getProviderId();
		
		service.insertSensors(message);
				
		verify(restClient).post(path, "");				
	}
	
	@Test
	public void updateSensorsOrComponents(){
		when(message.getSensors()).thenReturn(new ArrayList<CatalogSensor>());
		when(message.getProviderId()).thenReturn("provider1");
		when(message.getBody()).thenReturn("");
		String path = "api/provider/"+message.getProviderId();
		
		service.updateSensorsOrComponents(message);
				
		verify(restClient).put(path, "");				
	}
	
	@Test
	public void getCredentials(){		
		String path = "api/credentials";		
		service.getCredentials();
				
		verify(restClient).get(path);				
	}
	
	@Test
	public void getPermissions(){		
		String path = "api/permissions";		
		service.getPermissions();
				
		verify(restClient).get(path);				
	}
	
	@Test
	public void getAlarmsOwners(){		
		String path = "api/alerts";		
		service.getAlertsOwners();
				
		verify(restClient).get(path);				
	}
	
	
}
