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
package org.sentilo.platform.server.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.http.HttpVersion;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.message.BasicHttpResponse;
import org.apache.http.message.BasicStatusLine;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.parser.DataParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;

import org.sentilo.common.utils.DateUtils;

public class DataParserTest {
	
	private DataParser parser;	
	@Mock private SentiloRequest sentiloRequest;	
	@Mock private SentiloResource resource;
	
	@Before
	public void setUp() throws Exception{				
		MockitoAnnotations.initMocks(this);		
		parser = new DataParser();
		when(sentiloRequest.getResource()).thenReturn(resource);
	}

	@Test
	public void parseDeleteRequest() throws Exception {
		String providerId = "prov1";
		String sensorId = "sensor1";
		
		when(resource.getResourcePart(0)).thenReturn(providerId);
		when(resource.getResourcePart(1)).thenReturn(sensorId);
		
		DataInputMessage message = parser.parseDeleteRequest(sentiloRequest);
		
		assertEquals(message.getProviderId(),providerId);
		assertEquals(message.getSensorId(),sensorId);
	}
	
	@Test
	public void parseGetRequestWithoutFilter() throws Exception{
		String providerId = "prov1";
		String sensorId = "sensor1";
		
		when(resource.getResourcePart(0)).thenReturn(providerId);
		when(resource.getResourcePart(1)).thenReturn(sensorId);
		
		DataInputMessage message = parser.parseGetRequest(sentiloRequest);
		
		assertEquals(message.getProviderId(),providerId);
		assertEquals(message.getSensorId(),sensorId);		
	}
	
	@Test
	public void parseGetRequestWithFilter() throws Exception{
		String providerId = "prov1";
		String sensorId = "sensor1";
		String from = "17/09/2012T12:34:45";
		String to = null;
		String limit = "5";
		
		when(resource.getResourcePart(0)).thenReturn(providerId);
		when(resource.getResourcePart(1)).thenReturn(sensorId);
		when(sentiloRequest.getRequestParameter("from")).thenReturn(from);
		when(sentiloRequest.getRequestParameter("to")).thenReturn(to);
		when(sentiloRequest.getRequestParameter("limit")).thenReturn(limit);
		
		DataInputMessage message = parser.parseGetRequest(sentiloRequest);
		
		assertEquals(message.getProviderId(),providerId);
		assertEquals(message.getSensorId(),sensorId);
		assertTrue(message.hasQueryFilters());
		assertEquals(message.getQueryFilters().getLimit(),parser.parseInteger(limit));
		assertEquals(message.getQueryFilters().getFrom(),parser.parseDate(from));
		assertEquals(message.getQueryFilters().getTo(),parser.parseDate(to));
	}
	
	@Test
	public void parsePutObservationsList() throws Exception {
		String json = "{\"observations\":[{\"value\":\"10.1\"}, {\"value\":\"11.2\", \"timestamp\": \"17/09/2012T12:34:45\"}, {\"value\":\"12.3\"}]}";
		String[] parts = {"prov1","sensor1"};
		
		when(sentiloRequest.getBody()).thenReturn(json);
		when(resource.getParts()).thenReturn(parts);
		
		DataInputMessage message = parser.parsePutRequest(sentiloRequest);
		assertEquals("Must parse 3 elements", 3, message.getObservations().size());		
	}
	
	@Test
	public void parsePutSimpleObservation() throws Exception {
		String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"17/09/2012T12:34:45\"}]}";
		String[] parts = {"prov1","sensor1"};
		
		when(sentiloRequest.getBody()).thenReturn(json);
		when(resource.getParts()).thenReturn(parts);
		
		DataInputMessage message = parser.parsePutRequest(sentiloRequest);
		assertEquals("Must parse 1 element", 1, message.getObservations().size());		
	}
	
	@Test(expected=PlatformException.class)
	public void parsePutBadRequestObservation() throws Exception {
		String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"17/09/2012A12:34:45\"}]}";
		String[] parts = {"prov1","sensor1"};
		
		when(sentiloRequest.getBody()).thenReturn(json);
		when(resource.getParts()).thenReturn(parts);
		
		parser.parsePutRequest(sentiloRequest);		
	}
	
	@Test(expected=PlatformException.class)
	public void parsePutObservationWithTrasposeTimestamp() throws Exception {
		String json = "{\"observations\":[{\"value\":\"11.2\", \"timestamp\": \"11/23/2012T12:34:45\"}]}";
		String[] parts = {"prov1","sensor1"};
		
		when(sentiloRequest.getBody()).thenReturn(json);
		when(resource.getParts()).thenReturn(parts);
		
		parser.parsePutRequest(sentiloRequest);		
	}
	
	@Test
	public void parsePutSensorsList() throws Exception {
		String json = "{\"sensors\":[{\"sensor\":\"sensor1\",\"observations\":[{\"value\":\"10.1\"},{\"value\":\"11.2\",\"timestamp\":\"17/09/2012T12:34:45\"},{\"value\":\"12.3\"}]},{\"sensor\":\"sensor2\",\"observations\":[{\"value\":\"10.1\"},{\"value\":\"11.2\",\"timestamp\":\"17/09/2012T12:34:45\"},{\"value\":\"12.3\"}]}]}";
		String[] parts = {"prov1"};
		
		when(sentiloRequest.getBody()).thenReturn(json);
		when(resource.getParts()).thenReturn(parts);
				
		DataInputMessage message = parser.parsePutRequest(sentiloRequest);		
		assertEquals("Must parse 6 element", 6, message.getObservations().size());		
	}
	
	@Test
	public void parseSensorWriteResponse() throws Exception {				
		String[] parts = {"prov1","sensor1"};				
		when(resource.getParts()).thenReturn(parts);
		
		SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));								
		parser.writeResponse(sentiloRequest, response, getObservationsFromSensor());					
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		((ByteArrayEntity)response.getHttpResponse().getEntity()).writeTo(baos);
		String expected = "{\"observations\":[{\"value\":\"1\",\"timestamp\":\"21/02/2013T17:49:24\"},{\"value\":\"10\",\"timestamp\":\"21/02/2013T17:49:30\"}]}";		
		assertEquals(expected, baos.toString());		
	}		
	
	@Test
	public void parseEmptySensorWriteResponse() throws Exception {				
		String[] parts = {"prov1","sensor1"};				
		when(resource.getParts()).thenReturn(parts);
		
		List<Observation> subscriptionList = Collections.emptyList();
		SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));								
		parser.writeResponse(sentiloRequest, response, subscriptionList);					
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		((ByteArrayEntity)response.getHttpResponse().getEntity()).writeTo(baos);
		String expected = "{\"observations\":[]}";		
		assertEquals(expected, baos.toString());		
	}
	
	@Test
	public void parseProviderWriteResponse() throws Exception {				
		String[] parts = {"prov1"};				
		when(resource.getParts()).thenReturn(parts);
		
		SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));								
		parser.writeResponse(sentiloRequest, response, getObservationsFromProvider());					
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		((ByteArrayEntity)response.getHttpResponse().getEntity()).writeTo(baos);
		String expected = "{\"sensors\":[{\"sensor\":\"sensor1\",\"observations\":[{\"value\":\"1\",\"timestamp\":\"21/02/2013T17:49:24\"}]},{\"sensor\":\"sensor2\",\"observations\":[{\"value\":\"10\",\"timestamp\":\"21/02/2013T17:49:30\"},{\"value\":\"5\",\"timestamp\":\"20/02/2013T17:49:30\"}]}]}";
		assertEquals(expected, baos.toString());		
	}
	
	@Test
	public void parseEmptyProviderWriteResponse() throws Exception {				
		String[] parts = {"prov1"};				
		when(resource.getParts()).thenReturn(parts);
		
		List<Observation> subscriptionList = Collections.emptyList();
		SentiloResponse response = SentiloResponse.build(new BasicHttpResponse(new BasicStatusLine(HttpVersion.HTTP_1_0, 200, "")));								
		parser.writeResponse(sentiloRequest, response, subscriptionList);					
		
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		((ByteArrayEntity)response.getHttpResponse().getEntity()).writeTo(baos);
		String expected = "{\"sensors\":[]}";		
		assertEquals(expected, baos.toString());		
	}
	
	private List<Observation> getObservationsFromSensor(){
		List<Observation> observations = new ArrayList<Observation>();
		Observation obs1 = new Observation("prov1", "sensor1", "1", DateUtils.toMillis("21/02/2013T17:49:24"));
		Observation obs2 = new Observation("prov1", "sensor1", "10", DateUtils.toMillis("21/02/2013T17:49:30"));
		observations.add(obs1);
		observations.add(obs2);		
		return observations;
	}
	
	private List<Observation> getObservationsFromProvider(){
		List<Observation> observations = new ArrayList<Observation>();
		Observation obs1 = new Observation("prov1", "sensor1", "1", DateUtils.toMillis("21/02/2013T17:49:24"));
		Observation obs2 = new Observation("prov1", "sensor2", "10", DateUtils.toMillis("21/02/2013T17:49:30"));
		Observation obs3 = new Observation("prov1", "sensor2", "5", DateUtils.toMillis("20/02/2013T17:49:30"));
		observations.add(obs1);
		observations.add(obs2);		
		observations.add(obs3);
		return observations;
	}
}
