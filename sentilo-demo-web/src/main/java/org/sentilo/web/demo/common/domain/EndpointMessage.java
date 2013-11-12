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
package org.sentilo.web.demo.common.domain;

import org.sentilo.common.utils.SentiloConstants;

/**
 * 
 * Representa un missatge que es rep per una subscripció.
 * Tot missatge que es rep via una subscripcio te el següent format: timestamp-content 
 *
 */
public class EndpointMessage {
	
	private String timestamp;
	private String message;
	private String topic;
	
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
	public String getTimestamp() {
		return timestamp;
	}
	public void setTimestamp(String timestamp) {
		this.timestamp = timestamp;
	}
	
	/**
	 * Retorna l'observació continguda en el missatge rebut
	 * @return l'observació
	 */
	public ObservationData getObservation(){
		ObservationData obs = new  ObservationData();
		String[] ids = topic.split(SentiloConstants.SLASH);
		
		obs.setProviderId(ids[ids.length-2]);
		obs.setSensorId(ids[ids.length-1]);
		obs.setValue(getMessage());
		obs.setTimestamp(getTimestamp());
		return obs;
	}
	
	/**
	 * Retorna l'alarma continguda en el missatge rebut
	 * @return l'alarma 
	 */
	public Alarm getAlarm(){
		Alarm alarm = new Alarm();
		String[] ids = topic.split(SentiloConstants.SLASH);
		alarm.setAlarmId(ids[ids.length-1]);
		alarm.setMessage(getMessage());
		alarm.setTimestamp(getMessage());
		return alarm;
	}
	
	/**
	 * Retorna l'ordre continguda en el missatge rebut
	 * @return l'ordre
	 */
	public Order getOrder(){
		Order order =  new Order();
		String[] ids = topic.split(SentiloConstants.SLASH);
		order.setProviderId(ids[ids.length-1]);
		order.setMessage(getMessage());
		order.setTimestamp(getMessage());
		return order;
	}
	
	@Override
	public String toString() {
		return "EndpointMessage [message=" + message + ", topic=" + topic + "]";
	}	

}
