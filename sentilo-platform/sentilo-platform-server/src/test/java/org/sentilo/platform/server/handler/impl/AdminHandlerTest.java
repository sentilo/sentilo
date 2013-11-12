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
package org.sentilo.platform.server.handler.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.http.HttpStatus;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.domain.AdminInputMessage.AdminType;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.AdminService;
import org.sentilo.platform.server.handler.impl.AdminHandler;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.parser.AdminParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.request.RequestUtils;
import org.sentilo.platform.server.response.SentiloResponse;


public class AdminHandlerTest {

	private static final String PROVIDER1 = "provider1";
	private AdminHandler handler;
	@Mock private AdminService service;
	@Mock private SentiloRequest request;
	@Mock private SentiloResource resource;
	@Mock private SentiloResponse response;
	@Mock private AdminParser parser;
	@Mock private AdminInputMessage message;	

	@Before
	public void setUp() {
		MockitoAnnotations.initMocks(this);
		handler = new AdminHandler();		
		
		handler.setAdminService(service);
		handler.setAdminParser(parser);
		
		when(request.getResource()).thenReturn(resource);		
	}

	@Test	
	public void notAllowedStatsRequest() throws Exception{									
		when(parser.parseGetRequest(request)).thenReturn(message);
		when(message.getType()).thenReturn(AdminType.stats);
		
		simulateRequest(HttpMethod.GET, PROVIDER1, "/admin/stats");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertForbiddenCall(e);
		}
	}
	
	@Test	
	public void statsRequest() throws Exception{											
		Statistics stats = new Statistics();
		when(parser.parseGetRequest(request)).thenReturn(message);
		when(message.getType()).thenReturn(AdminType.stats);
		when(service.getStatistics()).thenReturn(stats);
		when(request.getEntitySource()).thenReturn("sentilo-catalog");
		
		simulateRequest(HttpMethod.GET, PROVIDER1, "/admin/stats");
		handler.manageRequest(request, response);
		
		verify(parser).parseGetRequest(request);
		verify(parser).writeResponse(request, response, stats);
	}
							
	private void assertForbiddenCall(PlatformException e) {
		assertEquals("Must return 403 - Method not allowed", HttpStatus.SC_FORBIDDEN, e.getHttpStatus());
	}		

	private void simulateRequest(HttpMethod method, String tokenProvider, String path) throws PlatformException{

		String[] splitted = RequestUtils.splitResource(RequestUtils.extractResource(path));
		when(resource.getParts()).thenReturn(splitted);				

		if (splitted.length > 0) {
			when(request.getResourcePart(0)).thenReturn(splitted[0]);
		}
		if (splitted.length > 1) {
			when(request.getResourcePart(1)).thenReturn(splitted[1]);
		}
		if (splitted.length > 2) {
			when(request.getResourcePart(2)).thenReturn(splitted[2]);
		}
		when(request.getMethod()).thenReturn(method);
		
		try{
			when(request.getBody()).thenReturn("");
		}catch(Exception e){
			throw new PlatformException(e);
		}
	}
	
}
