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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.dto.ObservationMessage;
import org.sentilo.platform.server.dto.ObservationsMessage;
import org.sentilo.platform.server.dto.SensorMessage;
import org.sentilo.platform.server.dto.SensorsMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.request.SentiloResource;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;


public class DataParser extends PlatformJsonMessageConverter {
	
	
	public DataInputMessage parsePutRequest(SentiloRequest request) throws PlatformException{				
		SentiloResource resource = request.getResource();
		List<Observation> observations = null;
		DataInputMessage message = null;
		if(resource.getParts().length==3){
			String providerId = resource.getResourcePart(0);
			String sensorId = resource.getResourcePart(1);
			String value = resource.getResourcePart(2);									
			Observation observation = new Observation(providerId, sensorId, value);
			observations = new ArrayList<Observation>();
			observations.add(observation);	
			message = new DataInputMessage(providerId, sensorId, observations);
			
		}else if(resource.getParts().length==1){
			String providerId = resource.getResourcePart(0);
			SensorsMessage inputMessage = (SensorsMessage)readInternal(SensorsMessage.class, request);
			observations =  inputMessageToDomain(resource, inputMessage);
			message = new DataInputMessage(providerId, observations);
		}else{
			String providerId = resource.getResourcePart(0);
			String sensorId = resource.getResourcePart(1);
			ObservationsMessage inputMessage = (ObservationsMessage)readInternal(ObservationsMessage.class, request);
			observations = inputMessageToDomain(resource, inputMessage);
			message = new DataInputMessage(providerId, sensorId, observations);
		}		
		
		return message;
	}
	
	public DataInputMessage parseDeleteRequest(SentiloRequest request) throws PlatformException{				
		SentiloResource resource = request.getResource();
		String providerId = resource.getResourcePart(0);
		String sensorId = resource.getResourcePart(1);
		
		return new DataInputMessage(providerId, sensorId);				
	}
	
	public DataInputMessage parseGetRequest(SentiloRequest request) throws PlatformException{				
		SentiloResource resource = request.getResource();
		String providerId = resource.getResourcePart(0);
		String sensorId = resource.getResourcePart(1);		
		String from = request.getRequestParameter("from"); 
		String to = request.getRequestParameter("to");
		String limit = request.getRequestParameter("limit");
				
		return new DataInputMessage(providerId, sensorId, parseDate(from), parseDate(to), parseInteger(limit));				
	}
	
	
	public void writeResponse(SentiloRequest request, SentiloResponse response, List<Observation> observations) throws PlatformException{
		//transformar a objeto de tipo SensorsMessage o ObservationsMessage, depende del caso de la petición
		Object message = parseObservationsListToMessage(request, observations);
		
		try{
			writeInternal(message, response);
		}catch(IOException ex){
			throw new PlatformException(HttpStatus.SC_INTERNAL_SERVER_ERROR, ex);
		}				
	}
	
	private Object parseObservationsListToMessage(SentiloRequest request, List<Observation> observations){
		SentiloResource resource = request.getResource();
		
		if(resource.getParts().length==1){						
			return parseObservationsListToSensorsMessage(observations);
		}else{			
			return parseObservationsListToObservationsMessage(observations);			
		}
	}
	
	private SensorsMessage parseObservationsListToSensorsMessage(List<Observation> observationsList){
		SensorsMessage sensorsMessage = new SensorsMessage();
		Map<String, SensorMessage> sensors = new HashMap<String, SensorMessage>();
		
		for(Observation observation : observationsList){
			ObservationMessage obsMessage = parseObservationToObservationMessage(observation);
			SensorMessage sensorMessage = sensors.get(observation.getSensor());
			if(sensorMessage == null){
				sensorMessage = new SensorMessage();
				sensorMessage.setSensor(observation.getSensor());
				sensors.put(sensorMessage.getSensor(), sensorMessage);
				sensorsMessage.addSensor(sensorMessage);
			}			
			sensorMessage.addObservationMessage(obsMessage);
		}
		
		return sensorsMessage;				
	}
			
	private ObservationsMessage parseObservationsListToObservationsMessage(List<Observation> observationsList){
		ObservationsMessage observations = new ObservationsMessage();
		for(Observation observation : observationsList){
			observations.addObservation(parseObservationToObservationMessage(observation));
		}
		
		return observations;
	}
	
	private ObservationMessage parseObservationToObservationMessage(Observation observation){
		ObservationMessage message = new ObservationMessage();
		message.setValue(observation.getValue());
		message.setLocation(observation.getLocation());
		message.setTimestamp(timestampToString(observation.getTimestamp()));
		
		return message;
	}
	
	
	
	private List<Observation> inputMessageToDomain(SentiloResource resource, SensorsMessage inputMessage) throws PlatformException{
		String providerId = resource.getResourcePart(0);						
		List<Observation> observations = new ArrayList<Observation>();
		
		for(SensorMessage sensorMessage: inputMessage.getSensors()){
			String globalLocation = sensorMessage.getLocation();
			String sensorId = sensorMessage.getSensor();
			
			observations.addAll(parseObservationsMessageToDomain(sensorMessage.getObservations(), globalLocation, providerId, sensorId));
		}
				
		return observations;				
	}
	
	private List<Observation> inputMessageToDomain(SentiloResource resource, ObservationsMessage inputMessage) throws PlatformException{
		String providerId = resource.getResourcePart(0);
		String sensorId = resource.getResourcePart(1);
		String globalLocation = inputMessage.getLocation();
		
		return parseObservationsMessageToDomain(inputMessage.getObservations(), globalLocation, providerId, sensorId);
	}
	
	private List<Observation> parseObservationsMessageToDomain(List<ObservationMessage> inputMessage, String globalLocation, String providerId, String sensorId) throws PlatformException{
		List<Observation> observations = new ArrayList<Observation>();
		
		for(ObservationMessage message: inputMessage){
			String value = message.getValue();
			String timestamp = message.getTimestamp();
			String location = (StringUtils.hasText(message.getLocation())?message.getLocation():globalLocation);
			
			Observation observation = new Observation(providerId, sensorId, value, parseTimestamp(timestamp), location);
			observations.add(observation);						
		}
				
		return observations;
	}		
}
