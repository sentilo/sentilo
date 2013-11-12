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

import org.sentilo.common.domain.AlertOwnerMessage;
import org.sentilo.common.domain.AlertsOwnersResponseMessage;
import org.sentilo.platform.common.domain.Alarm;
import org.sentilo.platform.common.domain.AlarmInputMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.AlarmService;
import org.sentilo.platform.common.service.CatalogService;
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
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;


@Service
public class AlarmServiceImpl implements AlarmService {
	
	private final Logger logger = LoggerFactory.getLogger(AlarmServiceImpl.class);
	
	@Autowired
	private JedisTemplate<String, String> jedisTemplate;
	
	@Autowired
	private JedisSequenceUtils jedisSequenceUtils;
	
	@Autowired
	private CatalogService catalogService;
	
	@Autowired
	private ResourceService resourceService;
	
	private Map<String, String> alertsOwners = new HashMap<String, String>();
	
    /*
     * (non-Javadoc)
     * @see org.sentilo.platform.common.service.AlarmService#setAlarm(org.sentilo.platform.common.domain.AlarmInputMessage)
     */
	public void setAlarm(AlarmInputMessage message){							
		// Si la alarma no existe, la registramos en Redis			
		registerAlarmIfNecessary(message);		
		// Registramos en Redis el mensaje asociado a la alarma		
		registerAlarmMessage(message);
		// Y por ultimo, publicamos el mensaje de la alarma
		publish(message);
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.AlarmService#getAlarmOwner(java.lang.String)
	 */
	public String getAlertOwner(String alertId){
		return alertsOwners.get(alertId);
	}
	
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.AlarmService#getLastMessages(org.sentilo.platform.common.domain.AlarmInputMessage)
	 */
	public List<Alarm> getLastMessages(AlarmInputMessage message){
		// Para recuperar los mensajes asociados a una alarma, debemos hacer lo siguiente:
		// 1. Recuperar el identificador interno de la alarma en Redis
		// 2. Si este no es null, recuperar los mensajes asociados a esta alarma que cumplen el criterio de busqueda				
		List<Alarm> messages = new ArrayList<Alarm>();
		Long aid = jedisSequenceUtils.getAid(message.getAlarmId());
		if(aid != null){
			messages.addAll(getLastAlarms(aid.toString(), message));
		}
				
		return messages;
	}
	
	private void registerAlarmIfNecessary(AlarmInputMessage message){
		// Si la alarma aun no esta registrada en Redis, la registramos.
		resourceService.registerAlarmIfNecessary(message.getAlarmId());		
	}
	
	private void registerAlarmMessage(AlarmInputMessage message){
		Long aid = jedisSequenceUtils.getAid(message.getAlarmId());
		Long amid = persistAlarmMessage(message);
		registerAlarmMessage(aid, amid, message);
	}
	
	private void registerAlarmMessage(Long aid, Long amid, AlarmInputMessage message){						
		Long timestamp = System.currentTimeMillis();
		// Definimos una reverse lookup key con la cual recuperar rapidamente los mensajes de una alarma			
		// A continuacion, añadimos el amid al Sorted Set alarm:{aid}:messages. La puntuacion, o score, que se asocia
		// a cada elemento del Set es el timestamp del mensaje.		
		jedisTemplate.zAdd("aid:"+aid+":messages", timestamp, amid.toString());
		
		logger.debug("Registered in Redis message {} for alarm {}", amid, message.getAlarmId());			
	}
	
	private Long persistAlarmMessage(AlarmInputMessage message){				
		Long amid = jedisSequenceUtils.getAmid();		
		
		Long timestamp = System.currentTimeMillis();
		
		// Guardamos una hash de clave amid:{amid} y valores source, message y timestamp.
		Map<String,String> fields = new HashMap<String, String>();
		fields.put("sender", message.getSender());
		fields.put("message",message.getMessage());
		fields.put("ts", timestamp.toString());
		jedisTemplate.hmSet("amid:"+amid, fields);		
		
		return amid;
	}
			
	private void publish(AlarmInputMessage message) {		
		logger.debug("Publish alarm event message {} associated with alarm {}", message.getMessage(), message.getAlarmId());
		Topic topic = ChannelUtils.buildTopic(PubSubChannelPrefix.alarm, message.getAlarmId());
		jedisTemplate.publish(topic.getTopic(),PublishMessageUtils.buildContentToPublish(message));		
	}
	
	
	
	private List<Alarm> getLastAlarms(String aid, AlarmInputMessage message){
		Long to = QueryFilterParamsUtils.getTo(message);
		Long from = QueryFilterParamsUtils.getFrom(message);
		Integer limit = QueryFilterParamsUtils.getLimit(message);
		
		// La sentencia a utilizar en Redis es:
		//     ZREVRANGEBYSCORE aid:{aid}:messages to from LIMIT 0 limit
									
		Set<String> amids = jedisTemplate.zRevRangeByScore("aid:"+aid+":messages", to, from, 0, limit);
		List<Alarm> alarmMessages = null;		
		
		if(!CollectionUtils.isEmpty(amids)){
			alarmMessages = getAlarms(amids, message.getAlarmId());
		}
		
		return alarmMessages;
	}
	
	private List<Alarm> getAlarms(Set<String> amids, String alarmId){
		List<Alarm> alarmMessages = new ArrayList<Alarm>();
		Iterator<String> it = amids.iterator();
		
		while(it.hasNext()){
			Long amid = Long.parseLong(it.next());
			Alarm alarm = getAlarm(amid, alarmId);			
			if(alarm != null){				
				alarmMessages.add(alarm);
			}			
		}		
		return alarmMessages;
	}
	
	private Alarm getAlarm(Long amid, String alarmId){		
		Alarm alarm = null;		
		String message = null;
		String ts = null;
		String sender = null;
							
		Map<String, String> infoSoid = jedisTemplate.hGetAll("amid:"+amid);
		if(!CollectionUtils.isEmpty(infoSoid)){
			message = infoSoid.get("message");
			ts = infoSoid.get("ts");
			sender = infoSoid.get("sender");
			
			alarm = new Alarm(alarmId, message, sender, Long.parseLong(ts));
		}														
		
		return alarm;		
	}
	
	
	
	@Scheduled(initialDelay=5000, fixedRate=900000)
	public void loadAlertsOwners() {		
		try {
			logger.debug("Actualizando cache de propietarios de alertas");			
			AlertsOwnersResponseMessage owners = catalogService.getAlertsOwners();			
			Map<String, String> auxAlertsOwners = new HashMap<String, String>();
			if(owners!=null && !CollectionUtils.isEmpty(owners.getAlerts())){
				for(AlertOwnerMessage alarmOwner:owners.getAlerts()){
					auxAlertsOwners.put(alarmOwner.getAlertId(), alarmOwner.getOwnerEntityId());
				}
			}
			
			replaceActiveAlertsOwners(auxAlertsOwners);
			
		} catch (CatalogAccessException e) {			
			logger.warn("Error al llamar al catalogo para recuperar la lista de propietarios de alarmas", e);
		} 	
	}
	
	private void replaceActiveAlertsOwners(Map<String, String> updatedAlertsOwners){
		alertsOwners.clear();
		alertsOwners.putAll(updatedAlertsOwners);
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
