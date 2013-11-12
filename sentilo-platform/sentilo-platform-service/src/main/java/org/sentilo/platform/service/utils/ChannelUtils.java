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
package org.sentilo.platform.service.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.sentilo.common.domain.SubscribeType;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.AlarmSubscription;
import org.sentilo.platform.common.domain.DataSubscription;
import org.sentilo.platform.common.domain.Observation;
import org.sentilo.platform.common.domain.OrderSubscription;
import org.sentilo.platform.common.domain.Subscription;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;


public final class ChannelUtils {
	
	public static enum PubSubChannelPrefix { data, order, alarm};	    	    	
		
	static final String TOKEN = PubSubConstants.REDIS_KEY_TOKEN;
	
	private ChannelUtils(){
		throw new AssertionError();
	}
	
	public static PubSubChannelPrefix translateSubscriptionType(SubscribeType subscribeType){
		return PubSubChannelPrefix.valueOf(subscribeType.toString().toLowerCase());
	}
	
	public static List<String> filterTopicsOfType(Set<String> topics, SubscribeType type){
		List<String> filteredTopics = new ArrayList<String>();
		if(!CollectionUtils.isEmpty(topics)){	
			Iterator<String> it = topics.iterator();
			
			while(it.hasNext()){
				String topicName = it.next();
				if(isTopicOfType(topicName, type)){
					filteredTopics.add(topicName);
				}
			}
		}
		
		return filteredTopics;
	}
	
	public static boolean isTopicOfType(String topicName, SubscribeType type){				
		return (type!=null && topicName.startsWith(type.toString().toLowerCase()));		
	}
	
	public static Topic getChannel(Observation observation){
		Assert.notNull(observation,"observation must not be null");
		Assert.notNull(observation.getProvider(),"An observation must have a provider"); 
		Assert.notNull(observation.getSensor(),"An observation must have a sensor"); 		
		return buildTopic(PubSubChannelPrefix.data, observation.getProvider(), observation.getSensor());		
	}
	
	public static Topic getChannel(Subscription subscription){
		Assert.notNull(subscription);
		switch(subscription.getType()){
			case DATA:
				return buildTopic(PubSubChannelPrefix.data, ((DataSubscription)subscription).getProviderId(), ((DataSubscription)subscription).getSensorId());				
			case ORDER:							
				return buildTopic(PubSubChannelPrefix.order, ((OrderSubscription)subscription).getOwnerEntityId(), ((OrderSubscription)subscription).getSensorId());
			case ALARM:					
				return buildTopic(PubSubChannelPrefix.alarm, ((AlarmSubscription)subscription).getAlertId());
			default:
				throw new IllegalArgumentException("Unknown subscription type:" + subscription.getType());
		}					
	}
	
	public static Subscription getSubscription(String entityId, String channel, String endpoint){
		Assert.notNull(channel);
		
		String[] tokens = channel.split(PubSubConstants.REDIS_KEY_TOKEN);
		// La llamada a valueOf ya lanza una illegalArgumentException en caso de que el valor a transformar no corresponda
		// a ningun valor de la enumeracion
		PubSubChannelPrefix type = PubSubChannelPrefix.valueOf(tokens[0]);
		Subscription subscription = null;
		String providerId, sensorId;
		switch(type){
			case data:				
				providerId = tokens[1]; //validar si es pattern, si viene el sensorId informado, ...
				sensorId = null;
				if(!providerId.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX) && tokens.length > 2){
					sensorId = tokens[2];
				}				
				subscription = new DataSubscription(entityId, endpoint, providerId, sensorId);
				break;
			case order:
				providerId = tokens[1]; //validar si es pattern, si viene el sensorId informado, ...
				sensorId = null;
				if(!providerId.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX) && tokens.length > 2){
					sensorId = tokens[2];
				}
				subscription = new OrderSubscription(entityId, providerId, sensorId, endpoint);
				break;
			case alarm:
				String alarmId = tokens[1];				
				subscription = new AlarmSubscription(entityId, null, endpoint, alarmId);
				break;
		}
		
		return subscription;
	}
						
	
	public static Topic buildTopic(PubSubChannelPrefix prefix, String...resources){
		// El atributo length puede tener 1 o 2 elementos. En caso de tener longitud 2 y que el segundo sea null
		// este metodo debe retornar un Topic de tipo Pattern y no Channel, ya que significa que queremos construir un Topic 
		// para hacer una publicacion masiva.
		
		StringBuilder sb = new StringBuilder(prefix.toString());		
		for(String resource:resources){
			if(StringUtils.hasText(resource)){
				sb.append(PubSubConstants.REDIS_KEY_TOKEN);
				sb.append(resource);
			}else if(resources.length == 2){
				sb.append(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
			}
		}
		
		return buildTopic(sb.toString());										
	}	
	
	public static Topic buildTopic(String topicName){
		if(isTopicPattern(topicName)){
			return new PatternTopic(topicName);
		}else{
			return new ChannelTopic(topicName);
		}
	}
	
	public static boolean isTopicPattern(String topic){
		return StringUtils.hasText(topic) && topic.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);
	}
	
	public static String channelToPattern(String channel){
		String pattern = channel;
		
		if(!isTopicPattern(channel)){
			int lastPos = channel.lastIndexOf(PubSubConstants.REDIS_KEY_TOKEN);
			pattern = channel.substring(0, lastPos).concat(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX);			
		}
		
		return pattern;
	}
	
	
	
	public static String buildSubscriptionPathFromTopic(String topicName){
		// Este metodo reconstruye el path asociado a un topic, teniendo presente que los paths no tienen el catacter *
		// lo que implica que deberemos eliminarlo de este.
		
		String[] tokens = topicName.split(PubSubConstants.REDIS_KEY_TOKEN);
		StringBuilder channelPath = new StringBuilder();
		for(String token:tokens){
			channelPath.append(SentiloConstants.SLASH);
			if(token.endsWith(PubSubConstants.REDIS_CHANNEL_PATTERN_SUFFIX)){
				//Eliminamos el caracter de patron
				channelPath.append(token.substring(0, token.length()-1));
			}else{
				channelPath.append(token);
			}						
		}
		
		return channelPath.toString();
	}
}
