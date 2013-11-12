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
package org.sentilo.agent.alert.listener;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.agent.alert.domain.Alarm;
import org.sentilo.agent.alert.trigger.TriggerEvaluator;
import org.sentilo.agent.alert.trigger.TriggerResult;
import org.sentilo.agent.alert.utils.AlertUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import org.sentilo.common.utils.SentiloConstants;

public class MessageListenerImpl implements MessageListener {
	private final Logger logger = LoggerFactory.getLogger(MessageListenerImpl.class);	
		
	private String name;	
	private RedisSerializer<String> serializer = new StringRedisSerializer();	
	
	private RedisTemplate<String, String> redisTemplate;
	private TriggerEvaluator triggerEvaluator;
		
	private List<Alarm> alarms;	
	
	private boolean validValue;
		
	
	public MessageListenerImpl(String name){
		super();
		Assert.notNull(name, "name must not be NULL") ;			
		this.name = name;
		this.triggerEvaluator = new TriggerEvaluator();
	}
	
	public MessageListenerImpl(String name, RedisTemplate<String, String> redisTemplate){
		this(name);		
		Assert.notNull(redisTemplate,"redisTemplate must not be NULL");						
		this.redisTemplate = redisTemplate;
		
	}
			
	public void onMessage(Message message, byte[] pattern) {
		String info = getInfo(message);
		String channel = getChannel(message);
				
		logger.debug("{} -->  Recibido mensaje en el canal {}", name, channel);
		logger.debug("{} -->  Contenido del mensaje {}", name, info);
		
		// En cada mensaje recibido debemos volver a inicializar el valor de este flag
		validValue = true;
		
		//El mensaje tiene el formato timestamp#@#value por lo que debemos extraer el valor del mensaje recibido
		// para poder evaluar las diferentes expresiones de las alarmas.
		String[] parts = info.split(SentiloConstants.NOTIFICATION_MESSAGE_TOKEN);			
		String value = parts[1];
		
		// Cada mensaje recibido debe ser evaluado por cada una de las alarmas asociadas al listener
		if(!CollectionUtils.isEmpty(getAlarms())){								
			for(Alarm alarm : getAlarms()){								
				evaluateMessage(alarm, value);								
			}						
		}	
		
		// Si el valor recibido no lanza ninguna alarma, lo persistimos como ultimo valor recibido.
		if(validValue){
			triggerEvaluator.setLastAcceptedValue(value);
		}
	}
	
	public void checkFrozenAlarm(){
		logger.debug("{} -->  check frozen alarms", name);
		if(!CollectionUtils.isEmpty(getAlarms())){
			for(Alarm alarm : getAlarms()){								
				switch(alarm.getTrigger()){
					case FROZEN:
						checkFrozen(alarm);
						break;
					default:	
						break;
				}
			}
		}
	}
	
	public void updateAlarms(List<Alarm> newAlarms){		
		this.alarms = newAlarms;		
	}
	
	public List<Alarm> getAlarms(){		
		return this.alarms;		
	}
	
	public String getName() {
		return name;
	}
	
	public void addAlarm(Alarm alarm){
		if(alarms == null){
			alarms = new ArrayList<Alarm>();
		}
		
		alarms.add(alarm);
	}
				
	protected String getInfo(Message message){
		return serializer.deserialize(message.getBody());
	}
	
	protected String getChannel(Message message){
		return serializer.deserialize(message.getChannel());
	}		
	
	private void evaluateMessage(final Alarm alarm, String value){
		TriggerResult result = triggerEvaluator.evaluate(alarm, value);
		if(result.triggerConditionChecked()){
			validValue = false;
			buildAndSendMessage(alarm, result);
		}		
	}
	
	private void checkFrozen(final Alarm alarm){
		TriggerResult result = triggerEvaluator.checkFrozen(alarm);
		if(result.triggerConditionChecked()){			
			buildAndSendMessage(alarm, result);
		}		
	}
	
	private void buildAndSendMessage(final Alarm alarm, final TriggerResult result){
		String channel = AlertUtils.buildTopicToPublishAlarm(alarm);
		String message = AlertUtils.buildMessageToPublish(result.getAlarmMessage());
		logger.debug("Publish alarm message [{}] into channel [{}]", message, channel);
		redisTemplate.convertAndSend(channel, message);		
	}

	public void setRedisTemplate(RedisTemplate<String, String> redisTemplate) {
		this.redisTemplate = redisTemplate;
	}
	
}
