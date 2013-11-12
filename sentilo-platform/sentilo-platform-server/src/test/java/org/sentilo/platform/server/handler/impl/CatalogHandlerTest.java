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
import static org.mockito.Matchers.any;
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
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.handler.impl.CatalogHandler;
import org.sentilo.platform.server.http.HttpMethod;
import org.sentilo.platform.server.parser.CatalogParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.request.RequestUtils;
import org.sentilo.platform.server.response.SentiloResponse;

import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.CatalogSensor;

public class CatalogHandlerTest {

	private static final String PROVIDER1 = "provider1";
	private CatalogHandler handler;
	@Mock private CatalogService service;
	@Mock private SentiloRequest request;
	@Mock private SentiloResource resource;
	@Mock private SentiloResponse response;
	@Mock private CatalogParser parser;
	@Mock private CatalogInputMessage message;
	@Mock private CatalogDeleteInputMessage deleteMessage;
	@Mock private AuthorizationService authorizationService;

	@Before
	public void setUp() {
		MockitoAnnotations.initMocks(this);
		handler = new CatalogHandler();	
		CatalogResponseMessage responseMessage = new CatalogResponseMessage();
		
		handler.setCatalogService(service);
		handler.setCatalogParser(parser);
		handler.setAuthorizationService(authorizationService);
		when(request.getResource()).thenReturn(resource);
		when(authorizationService.hasAccessToRead(anyString(), anyString())).thenReturn(true);
		when(authorizationService.hasAccessToWrite(anyString(), anyString())).thenReturn(true);
		when(service.insertSensors(any(CatalogInputMessage.class))).thenReturn(responseMessage);
		when(service.updateSensorsOrComponents(any(CatalogInputMessage.class))).thenReturn(responseMessage);
		when(service.getAuthorizedProviders(any(CatalogInputMessage.class))).thenReturn(responseMessage);
		when(service.deleteProvider(any(CatalogInputMessage.class))).thenReturn(responseMessage);
	}

	@Test	
	public void getRequest() throws Exception{									
		List<CatalogSensor> sensors = getSensors();
		String body = "lo que sea";
		when(parser.parseGetRequest(request)).thenReturn(message);
		when(message.getSensors()).thenReturn(sensors);
		when(message.getBody()).thenReturn(body);		
		
		simulateRequest(HttpMethod.GET, PROVIDER1, "/catalog");
		handler.manageRequest(request, response);
		
		verify(parser).parseGetRequest(request);		
		verify(service).getAuthorizedProviders(message);
	}
	
	
	@Test	
	public void postRequest() throws Exception{									
		List<CatalogSensor> sensors = getSensors();
		String body = "lo que sea";
		when(parser.parsePostRequest(request)).thenReturn(message);
		when(message.getSensors()).thenReturn(sensors);
		when(message.getBody()).thenReturn(body);		
		
		simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
		handler.manageRequest(request, response);
		
		verify(parser).parsePostRequest(request);		
		verify(service).insertSensors(message);
	}
	
	@Test	
	public void deleteRequest() throws Exception{													
		when(parser.parseDeleteRequest(request,false)).thenReturn(deleteMessage);		
		//when(deleteMessage.getBody()).thenReturn(body);		
		
		simulateRequest(HttpMethod.DELETE, PROVIDER1, "/catalog/provider1");
		handler.manageRequest(request, response);
		
		verify(parser).parseDeleteRequest(request,false);		
		verify(service).deleteProvider(deleteMessage);
	}
	
	@Test	
	public void postRequestWithoutSensors() throws Exception{									
		when(parser.parsePostRequest(request)).thenReturn(message);
		
		simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertBadRequest(e);
		}		
	}
	
	@Test	
	public void postRequestWithInternalServerError() throws Exception{									
		List<CatalogSensor> sensors = getSensors();
		String body = "lo que sea";
		CatalogResponseMessage responseMessage = new CatalogResponseMessage("Error al insertar sensores");
		when(parser.parsePostRequest(request)).thenReturn(message);
		when(message.getSensors()).thenReturn(sensors);
		when(message.getBody()).thenReturn(body);
		when(service.insertSensors(message)).thenReturn(responseMessage);
		
		simulateRequest(HttpMethod.POST, PROVIDER1, "/catalog/provider1");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertInternalServerError(e);
		}	
	}
			
	@Test	
	public void putRequest() throws Exception{											
		List<CatalogSensor> sensors = getSensors();
		String body = "lo que sea";
		when(parser.parsePutRequest(request)).thenReturn(message);
		when(message.getSensors()).thenReturn(sensors);
		when(message.getBody()).thenReturn(body);		
		
		simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
		handler.manageRequest(request, response);
		
		verify(parser).parsePutRequest(request);		
		verify(service).updateSensorsOrComponents(message);
	}
	
	@Test	
	public void putRequestWithoutEntities() throws Exception{									
		when(parser.parsePutRequest(request)).thenReturn(message);
		
		simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertBadRequest(e);
		}		
	}
	
	@Test	
	public void putRequestWithInternalServerError() throws Exception{									
		List<CatalogSensor> sensors = getSensors();
		String body = "lo que sea";
		CatalogResponseMessage responseMessage = new CatalogResponseMessage("Error al actualizar sensores");
		when(parser.parsePutRequest(request)).thenReturn(message);
		when(message.getSensors()).thenReturn(sensors);
		when(message.getBody()).thenReturn(body);
		when(service.updateSensorsOrComponents(message)).thenReturn(responseMessage);
		
		simulateRequest(HttpMethod.PUT, PROVIDER1, "/catalog/provider1");
		try{
			handler.manageRequest(request, response);
		}catch (PlatformException e) {
			assertInternalServerError(e);
		}	
	}
						
	
	private void assertBadRequest(PlatformException e) {
		assertEquals("Must return 400 - Method not allowed", HttpStatus.SC_BAD_REQUEST, e.getHttpStatus());
	}
	
	private void assertInternalServerError(PlatformException e) {
		assertEquals("Must return 500 - Method not allowed", HttpStatus.SC_INTERNAL_SERVER_ERROR, e.getHttpStatus());
	}


	private void simulateRequest(HttpMethod method, String tokenProvider, String path) throws PlatformException{

		String[] splitted = RequestUtils.splitResource(RequestUtils.extractResource(path));
		when(resource.getParts()).thenReturn(splitted);				

		if(splitted!=null){
			if (splitted.length > 0) {
				when(request.getResourcePart(0)).thenReturn(splitted[0]);
			}
			if (splitted.length > 1) {
				when(request.getResourcePart(1)).thenReturn(splitted[1]);
			}
			if (splitted.length > 2) {
				when(request.getResourcePart(2)).thenReturn(splitted[2]);
			}
		}
		when(request.getMethod()).thenReturn(method);
		when(request.getEntitySource()).thenReturn(tokenProvider);
		try{
			when(request.getBody()).thenReturn("");
		}catch(Exception e){
			throw new PlatformException(e);
		}
	}
	
	private List<CatalogSensor> getSensors(){
		List<CatalogSensor> sensors = new ArrayList<CatalogSensor>();
		CatalogSensor sensor1 = buildSensor("sensor1", "prov1", "desc dels ensor1", "data", "35.5 67.8", "temperatura", "C");
		CatalogSensor sensor2 = buildSensor("sensor2", "prov1", "desc dels ensor2", "data", "35.5 67.8", "temperatura", "C");
		sensors.add(sensor1);
		sensors.add(sensor2);
		
		return sensors;
	}
	


	private CatalogSensor buildSensor(String sensor, String provider, String description, String dataType, String location, 
			String type, String unit) {		
		CatalogSensor catalogSensor = new CatalogSensor();
		catalogSensor.setSensor(sensor);
		catalogSensor.setProvider(provider);
		catalogSensor.setDescription(description);
		catalogSensor.setDataType(dataType);
		catalogSensor.setLocation(location);
		catalogSensor.setType(type);
		catalogSensor.setUnit(unit);
		
		return catalogSensor;
	}

	
}
