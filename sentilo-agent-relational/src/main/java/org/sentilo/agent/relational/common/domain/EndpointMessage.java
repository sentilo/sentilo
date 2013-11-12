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
package org.sentilo.agent.relational.common.domain;

import org.sentilo.agent.relational.utils.Constants;
import org.sentilo.common.utils.SentiloConstants;


/** 
 * Representa un missatge que es rep per una subscripció  
 */
public class EndpointMessage {
	
	private String message;
	private String timestamp;
	private String topic;
	
	public EndpointMessage(String info, String topic) {
		super();
		String[] parts = info.split(SentiloConstants.NOTIFICATION_MESSAGE_TOKEN);
		this.timestamp = parts[0];
		this.message = parts[1];		
		this.topic = topic;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public String getTopic() {
		return topic;
	}
	public void setTopic(String topic) {
		this.topic = topic;
	}	
	public void setTimestamp(String timestamp) {
		this.timestamp = timestamp;
	}
	public String getTimestamp() {
		return timestamp;
	}
	
	/**
	 * Retorna l'observació continguda en el missatge rebut
	 * @return l'observació
	 */
	public Observation getObservation(){
		Observation obs = new  Observation();
		
		// El topic siempre tiene el formato data:provider:sensor		
		String[] ids = topic.split(Constants.TOPIC_TOKEN);
		
		obs.setProvider(ids[1]);
		if(ids.length==3){
			obs.setSensor(ids[2]);
		}
				
		obs.setValue(message);
		obs.setTimestamp(timestamp);		
		return obs;
	}
	
	/**
	 * Retorna l'alarma continguda en el missatge rebut
	 * @return l'alarma 
	 */
	public Alarm getAlarm(){
		Alarm alarm = new Alarm();
		String[] ids = topic.split(Constants.TOPIC_TOKEN);
		alarm.setAlarm(ids[1]);
		alarm.setMessage(message);	
		alarm.setTimestamp(timestamp);		
		return alarm;
	}
	
	/**
	 * Retorna l'ordre continguda en el missatge rebut
	 * @return l'ordre
	 */
	public Order getOrder(){
		Order order =  new Order();
		String[] ids = topic.split(Constants.TOPIC_TOKEN);
		order.setProvider(ids[1]);
		if(ids.length == 3){			
			order.setSensor(ids[2]);
		}

		order.setMessage(message);
		order.setTimestamp(timestamp);		
		return order;
	}
	
	@Override
	public String toString() {
		return "EndpointMessage [message=" + message + ", topic=" + topic + ", timestamp=" + timestamp + "]";
	}
	


}
