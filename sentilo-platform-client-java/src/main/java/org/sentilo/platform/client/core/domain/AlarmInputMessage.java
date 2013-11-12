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
package org.sentilo.platform.client.core.domain;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.platform.client.core.utils.ResourcesUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;


public class AlarmInputMessage implements PlatformClientInputMessage, PlatformSearchInputMessage{
		
	private String alarmId;
	private Message message;
	
	private QueryFilterParams queryFilters;
	
	private String identityToken;
	
	/** Lista ordenada de los identificadores que forman el path del recurso. */
	private final List<String> resourcesValues = new ArrayList<String>();
	
	public AlarmInputMessage(String alarmId){
		super();
		
		Assert.isTrue(StringUtils.hasText(alarmId));		
		
		this.alarmId = alarmId;		
		
		ResourcesUtils.addToResources(alarmId, getResourcesValues());
	}
	
	public AlarmInputMessage(String alarmId, String message){
		this(alarmId);
				
		Assert.isTrue(StringUtils.hasText(message));
				
		this.message = new Message(message);		
	}
	
	
	public AlarmInputMessage(String alarmId, QueryFilterParams queryFilters) {
		this(alarmId);		
		this.queryFilters = queryFilters;
	}
			
	
	public String getIdentityToken() {
		return identityToken;
	}

	public void setIdentityToken(String identityToken) {
		this.identityToken = identityToken;
	}
	
	public String getAlarmId() {
		return alarmId;
	}

	public Message getMessage() {
		return message;
	}
		
	public List<String> getResourcesValues() {
		return resourcesValues;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#getQueryFilters()
	 */
	public QueryFilterParams getQueryFilters(){
		return queryFilters;
	}
		
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.client.core.domain.PlatformSearchInputMessage#hasQueryFilters()
	 */
	public boolean hasQueryFilters(){
		return queryFilters!=null;
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("\n\t --- Alarm ---");
		sb.append("\n\t alarm:" + alarmId);
		if(message!=null){
			sb.append("\n\t message:" + message.getMessage());
		}
		if(hasQueryFilters()){
			sb.append(getQueryFilters());
		}
		return sb.toString();
	}

	static class Message{
		private String _message;
		
		public Message(String message){
			this._message = message;
		}		

		public String getMessage() {
			return _message;
		}	
	}			
}

