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
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpStatus;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.Order;
import org.sentilo.platform.common.domain.OrderInputMessage;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.OrderService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.handler.impl.OrderHandler;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.parser.OrderParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.request.RequestUtils;
import org.sentilo.platform.server.response.SentiloResponse;


public class OrderHandlerTest {

	private static final String PROVIDER1 = "provider1";
	private OrderHandler handler;
	@Mock private OrderService service;
	@Mock private SentiloRequest request;
	@Mock private SentiloResource resource;
	@Mock private SentiloResponse response;
	@Mock private OrderParser parser;
	@Mock private OrderInputMessage message;
	@Mock private AuthorizationService authorizationService;

	@Before
	public void setUp() {
		MockitoAnnotations.initMocks(this);
		handler = new OrderHandler();				
		handler.setOrderService(service);
		handler.setOrderParser(parser);
		handler.setAuthorizationService(authorizationService);
		when(request.getResource()).thenReturn(resource);
		when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
		when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
	}

	@Test	
	public void putRequest() throws Exception{									
		when(parser.parseRequest(request)).thenReturn(message);
		when(message.getOrder()).thenReturn("stop");
		
		simulateRequest(HttpMethod.PUT, PROVIDER1, "/order/prov1/sensor1");
		handler.manageRequest(request, response);
		
		verify(parser).parseRequest(request);		
		verify(service).setOrder(message);		
	}
	
	@Test	
	public void invalidPutRequest() throws Exception{									
		when(parser.parseRequest(request)).thenReturn(message);
		when(message.getOrder()).thenReturn("stop");
		
		simulateRequest(HttpMethod.PUT, PROVIDER1, "/order/prov1/sensor1");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertBadRequest(e);
		}			
	}
	
	@Test	
	public void deleteRequest() throws Exception{									
		try {
			simulateRequest(HttpMethod.DELETE, PROVIDER1,	"/order/prov1/sensor1");
			handler.manageRequest(request, response);
		} catch (PlatformException e) {
			assertMethodNotAllowed(e);
		}
	}
	
	@Test	
	public void getRequest() throws Exception{							
		List<Order> orders = getOrders();
		OrderInputMessage message = new OrderInputMessage("provider2","sensor1");
		when(parser.parseGetRequest(request)).thenReturn(message);
		when(service.getLastOrders(message)).thenReturn(orders);
		
		simulateRequest(HttpMethod.GET, PROVIDER1, "/order/provider2/sensor1");
		handler.manageRequest(request, response);
		
		verify(parser).parseGetRequest(request);
		verify(parser).writeResponse(request, response, orders);
	}
	
	@Test	
	public void postRequest() throws Exception{											
		try {
			simulateRequest(HttpMethod.POST, PROVIDER1,	"/order/prov1/sensor1");
			handler.manageRequest(request, response);
		} catch (PlatformException e) {
			assertMethodNotAllowed(e);
		}
	}
				
	
	private void assertMethodNotAllowed(PlatformException e) {
		assertEquals("Must return 405 - Method not allowed", HttpStatus.SC_METHOD_NOT_ALLOWED, e.getHttpStatus());
	}
	
	private void assertBadRequest(PlatformException e) {
		assertEquals("Must return 400 - Method not allowed", HttpStatus.SC_BAD_REQUEST, e.getHttpStatus());
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
		when(request.getEntitySource()).thenReturn(tokenProvider);
		try{
			when(request.getBody()).thenReturn("");
		}catch(Exception e){
			throw new PlatformException(e);
		}
	}
	
	private List<Order> getOrders(){
		List<Order> orders = new ArrayList<Order>();		
		Order order1 = new Order("prov1", "sensor1", "stop", "sender1", System.currentTimeMillis());
		Order order2 = new Order("prov1", "sensor1", "start", "sender1", System.currentTimeMillis());
		orders.add(order1);
		orders.add(order2);
		
		return orders;
	}
	
}
