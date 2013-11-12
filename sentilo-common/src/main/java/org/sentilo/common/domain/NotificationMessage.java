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
package org.sentilo.common.domain;

import org.sentilo.common.utils.SentiloConstants;

/**
 * Aquesta classe conté l'estructura del missatge que s'envia als clients de les subscripcions a la plataforma. 
 */
public class NotificationMessage {
	
	private String message;
	private String timestamp;
	private String topic;
	
	public NotificationMessage(){
		
	}
	
	public NotificationMessage(String info, String topic){
		String[] parts = info.split(SentiloConstants.NOTIFICATION_MESSAGE_TOKEN);
		this.timestamp = parts[0];
		this.message = parts[1];		
		this.topic = topic;
	}
	
	public String toString(){
		StringBuilder sb = new StringBuilder("\n--- Notification --- ");
		sb.append("\n\t message:" + message);
		sb.append("\n\t timestamp:" + timestamp);
		sb.append("\n\t topic:" + topic);
		return sb.toString();
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

	public String getTimestamp() {
		return timestamp;
	}

	public void setTimestamp(String timestamp) {
		this.timestamp = timestamp;
	}

}
