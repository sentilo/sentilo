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
package org.sentilo.agent.relational.listener;

import org.sentilo.agent.relational.business.service.DataTrackService;
import org.sentilo.agent.relational.common.domain.Alarm;
import org.sentilo.agent.relational.common.domain.EndpointMessage;
import org.sentilo.agent.relational.common.domain.Observation;
import org.sentilo.agent.relational.common.domain.Order;
import org.sentilo.agent.relational.utils.Constants;
import org.sentilo.agent.relational.utils.ThreadLocalProperties;
import org.sentilo.common.domain.SubscribeType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import org.springframework.util.Assert;


public class MessageListenerImpl implements MessageListener {
	private final Logger logger = LoggerFactory.getLogger(MessageListenerImpl.class);	
	
	/** This name is equals to the dataSource name to use to persist the messages received by this listener*/
	private String name;	
	private RedisSerializer<String> serializer = new StringRedisSerializer();
	private DataTrackService dataTrackService;
			
	
	public MessageListenerImpl(String name){
		super();
		Assert.notNull(name,"name must not be NULL") ;
		this.name = name;			
	}
	
	public MessageListenerImpl(String name, DataTrackService dataTrackService){
		this(name);
		this.dataTrackService = dataTrackService;
	}
	
	public void onMessage(Message message, byte[] pattern) {
		String info = getInfo(message);
		String channel = getChannel(message);
		
		logger.debug("{} -->  Recibido mensaje en el canal {}", name, channel);
		logger.debug("{} -->  Contenido del mensaje {}", name, info);			
		
		ThreadLocalProperties.unset();
		ThreadLocalProperties.set(name);
		
		EndpointMessage endpointMessage = new EndpointMessage(info,channel);
		
		try {
			
			switch(getTopicType(channel)){
				case  DATA:
					Observation observation = endpointMessage.getObservation();
					observation.setTargetDs(name);
					dataTrackService.save(observation);
					break;
				case  ALARM:
					Alarm alarm = endpointMessage.getAlarm();
					alarm.setTargetDs(name);
					dataTrackService.save(alarm);
					break;
				case  ORDER:	
					Order order = endpointMessage.getOrder();
					order.setTargetDs(name);
					dataTrackService.save(order);
					break;
			}
						
		} catch (DataAccessException e) {
			logger.error("Error processing message {}. Error: {} ", info, e);
		}	
	}
	
	private SubscribeType getTopicType(String topic){
		String[] tokens = topic.split(Constants.REDIS_KEY_TOKEN);
		return SubscribeType.valueOf(tokens[0].toUpperCase());
	}
	
	protected String getInfo(Message message){
		return serializer.deserialize(message.getBody());
	}
	
	protected String getChannel(Message message){
		return serializer.deserialize(message.getChannel());
	}
	
	public String getName() {
		return name;
	}

	public DataTrackService getDataTrackService() {
		return dataTrackService;
	}

}
