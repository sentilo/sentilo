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
package org.sentilo.platform.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.Sensor;
import org.sentilo.platform.common.service.DataService;
import org.sentilo.platform.common.service.ResourceService;
import org.sentilo.platform.service.dao.JedisSequenceUtils;
import org.sentilo.platform.service.dao.JedisTemplate;
import org.sentilo.platform.service.utils.ChannelUtils;
import org.sentilo.platform.service.utils.PublishMessageUtils;
import org.sentilo.platform.service.utils.QueryFilterParamsUtils;
import org.sentilo.platform.service.utils.ChannelUtils.PubSubChannelPrefix;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


@Service
public class DataServiceImpl implements DataService {		
	
	private final Logger logger = LoggerFactory.getLogger(DataServiceImpl.class);	
	
	@Autowired
	private JedisTemplate<String, String> jedisTemplate;

	@Autowired
	private JedisSequenceUtils jedisSequenceUtils;
	
	@Autowired
	private ResourceService resourceService;
					
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.DataService#setObservations(org.sentilo.platform.common.domain.DataInputMessage)
	 */
	public void setObservations(DataInputMessage message) {
		List<Observation> observations = message.getObservations();
		for(Observation observation: observations){
			setObservation(observation);
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.DataService#deleteLastObservations(org.sentilo.platform.common.domain.DataInputMessage)
	 */
	public void deleteLastObservations(DataInputMessage message) {
		if(StringUtils.hasText(message.getSensorId())){
			deleteLastObservation(message.getProviderId(), message.getSensorId());
		}else{
			deleteLastObservations(message.getProviderId());
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.DataService#getLastObservations(org.sentilo.platform.common.domain.DataInputMessage)
	 */
	public List<Observation> getLastObservations(DataInputMessage message) {
		// Para recuperar las observaciones del sensor / sensores de un proveedor, debemos hacer lo siguiente:
		// 1. Recuperar los identificadores internos de los sensores de los cuales queremos recuperar las 
		//    observaciones.
		// 2. Para cada sensor, recuperar las observaciones que cumplen el criterio de busqueda				
		List<Observation> globalObservations = new ArrayList<Observation>();
		Set<String> sids = resourceService.getSensorsToInspect(message.getProviderId(), message.getSensorId());
		if(CollectionUtils.isEmpty(sids)){
			logger.debug("Provider {} has not sensors registered", message.getProviderId());
			return globalObservations;
		}
		
		logger.debug("Retrieving last observations for {} sensors of provider {}", sids.size(), message.getProviderId());				 
		
		Iterator<String> it = sids.iterator();
		while(it.hasNext()){
			String sid = it.next();
			List<Observation> observationsFromSensor = getLastObservations(sid, message);
			if(!CollectionUtils.isEmpty(observationsFromSensor)){
				globalObservations.addAll(observationsFromSensor);
			}
		}
				
		return globalObservations;
	}
	
	
	private void deleteLastObservations(String providerId) {				
		Set<String> sids = resourceService.getSensorsFromProvider(providerId);	
		
		if(CollectionUtils.isEmpty(sids)){
			logger.debug("Provider {} has not sensors registered", providerId);
			return;
		}
		
		logger.debug("Found {} sensors registered for provider {}", sids.size(), providerId);
		
		Iterator<String> it = sids.iterator();
		while(it.hasNext()){
			String sid = it.next();
			deleteLastObservation(new Long(sid));
			logger.debug("Removed last observation from sensor sid {} and provider {}", sid, providerId);
		}				
	}
	
	private void deleteLastObservation(String providerId, String sensorId) {													
		Long sid = jedisSequenceUtils.getSid(providerId, sensorId);
		if(sid == null){
			// Si no hay identificador interno del sensor, entonces este no tiene ninguna observacion registrada.
			return;
		}
		
		deleteLastObservation(sid);
		
		logger.debug("Removed last observation from sensor {} and provider {}", sensorId, providerId);
	}			
	
	private void setObservation(Observation data) {						
		// Si el proveedor y/o el sensor no existen, los registramos en Redis			
		registerProviderAndSensorIfNecessary(data);						
		// Registramos en Redis la observacion		
		registerSensorData(data);
		// Y por ultimo, publicamos la observacion
		publishSensorData(data);		
	}
	
	private List<Observation> getLastObservations(String sid, DataInputMessage message){
		Long to = QueryFilterParamsUtils.getTo(message);
		Long from = QueryFilterParamsUtils.getFrom(message);
		Integer limit = QueryFilterParamsUtils.getLimit(message);				
		
		// La sentencia a utilizar en Redis es:
		//     ZREVRANGEBYSCORE sid:{sid}:observations to from LIMIT 0 limit
										
		Set<String> sdids = jedisTemplate.zRevRangeByScore("sid:"+sid+":observations", to, from, 0, limit);
		List<Observation> observations = null;		
		
		if(!CollectionUtils.isEmpty(sdids)){
			observations = getObservations(sdids);
		}
		
		return observations;
	}
	
	private List<Observation> getObservations(Set<String> sdids){
		List<Observation> observations = new ArrayList<Observation>();
		Iterator<String> it = sdids.iterator();
		
		while(it.hasNext()){
			Long sdid = Long.parseLong(it.next());
			Observation observation = getObservation(sdid);
			if(observation != null){
				observations.add(observation);
			}			
		}		
		return observations;
	}
	
	private Observation getObservation(Long sdid){		
		Observation observation = null;
		String sid = null;
		String value = null;
		String ts = null;
		String location = null;
							
		Map<String, String> infoSdid = jedisTemplate.hGetAll("sdid:"+sdid);
		if(!CollectionUtils.isEmpty(infoSdid)){
			value = infoSdid.get("data");
			ts = infoSdid.get("ts");
			sid = infoSdid.get("sid");
			location = infoSdid.get("location");
		}										
				
		if(StringUtils.hasText(sid)){
			Sensor sensor = resourceService.getSensor(Long.parseLong(sid));
			observation = new Observation(sensor.getProvider(), sensor.getSensor(), value, Long.parseLong(ts), location);
		}
		
		return observation;		
	}		
		
		
		
	private void deleteLastObservation(Long sid){						
		// Para eliminar la ultima observacion de un sensor lo que debemos hacer es lo siguiente:
		//   1. Recuperamos el ultimo elemento del Sorted Set de observaciones del sensor (i.e., el que tiene score mas alto).
		//   2. Eliminamos este elemento del Sorted Set.			
		//   3. Eliminamos la clave sdid:{sdid}
		Set<String> sdids = jedisTemplate.zRange("sid:"+sid+":observations", -1, -1);				
		if(!CollectionUtils.isEmpty(sdids)){
			jedisTemplate.zRemRangeByRank("sid:"+sid+":observations", -1, -1);
			String sdid = sdids.iterator().next();
			jedisTemplate.del("sdid:"+sdid);
		}										
	}
	
	private void registerSensorData(Observation data){							
		Long sid = jedisSequenceUtils.getSid(data.getProvider(), data.getSensor());
		Long sdid = jedisSequenceUtils.getSdid();
										
		Long timestamp = data.getTimestamp();
		String location = (StringUtils.hasText(data.getLocation())?data.getLocation():"");
		// Guardamos una hash de clave sdid:{sdid} y valores sid, data (aleatorio), timestamp y location.
		Map<String,String> fields = new HashMap<String, String>();
		fields.put("sid", Long.toString(sid));
		fields.put("data",data.getValue());
		fields.put("ts", timestamp.toString());
		fields.put("location", location);
		jedisTemplate.hmSet("sdid:"+sdid, fields);			
					
		// Y definimos una reverse lookup key con la cual recuperar rapidamente las observaciones de un sensor			
		// A continuacion, añadimos el sdid al Sorted Set sensor:{sid}:observations. La puntuacion, o score, que se asocia
		// a cada elemento del Set es el timestamp de la observacion.		
		jedisTemplate.zAdd("sid:"+sid+":observations", timestamp, sdid.toString());
		
		logger.debug("Registered in Redis observation {} for sensor {} from provider {}", sdid, data.getSensor(), data.getProvider());		
	}
	
	private void publishSensorData(Observation data){											
		Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.data, data.getProvider(), data.getSensor());			
        jedisTemplate.publish(topic.getTopic(), PublishMessageUtils.buildContentToPublish(data));																																	
	}			
	
	private void registerProviderAndSensorIfNecessary(Observation data){
		// Si el proveedor y/o el sensor aun no estan registrados en Redis, los registramos.
		resourceService.registerProviderIfNecessary(data.getProvider());
		resourceService.registerSensorIfNecessary(data.getSensor(), data.getProvider());		
	}		

	public void setJedisTemplate(JedisTemplate<String, String> jedisTemplate) {
		this.jedisTemplate = jedisTemplate;
	}

	public void setJedisSequenceUtils(JedisSequenceUtils jedisSequenceUtils) {
		this.jedisSequenceUtils = jedisSequenceUtils;
	}
	
	public void setResourceService(ResourceService resourceService) {
		this.resourceService = resourceService;
	}
			
}
