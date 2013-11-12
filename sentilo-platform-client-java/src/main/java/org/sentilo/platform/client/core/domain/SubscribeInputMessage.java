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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.SubscribeType;
import org.sentilo.platform.client.core.utils.ResourcesUtils;
import org.springframework.util.CollectionUtils;


public class SubscribeInputMessage implements PlatformClientInputMessage{	    
    
    public static final String ALARM_ID_KEY = "alarmId";
    public static final String PROVIDER_ID_KEY = "providerId";
    public static final String SENSOR_ID_KEY = "sensorId";
	
	/** Endpoint al cual se debera enviar las notificaciones mediante Http callback*/
	protected Endpoint endpoint;
	
	/** Tipo de subscripcion. */
	protected SubscribeType type;	
	
	/** Identificadores del recurso al cual esta asociado la subscripción: providerId, sensorId o alarmId.*/
	protected final Map<String,String> resources = new HashMap<String, String>();
	
	/** Lista ordenada de los identificadores que forman el path del recurso. */
	protected final List<String> resourcesValues = new ArrayList<String>();
	
	private String identityToken;
	
	public SubscribeInputMessage(){
		this(null);
	}
	
	public SubscribeInputMessage(SubscribeType type){
		this(null, type);
	}
			
	public SubscribeInputMessage(Endpoint endpoint, SubscribeType type){
		super();		
		this.endpoint = endpoint;
		this.type = type;
	}					
	
	protected void addResource(String key, String value){
		this.resources.put(key, value);
		ResourcesUtils.addToResources(value, getResourcesValues());
	}
	
	public String getIdentityToken() {
		return identityToken;
	}

	public void setIdentityToken(String identityToken) {
		this.identityToken = identityToken;
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append("--- Subscription ---");
		sb.append("\n\t type:" + type);
		if(endpoint!=null){
			sb.append("\n\t endpoint:" + endpoint.getEndpoint());
		}
		if(!CollectionUtils.isEmpty(resources)){
			sb.append("\n\t Resources:");
			sb.append("\n\t\t" + resources.toString());
		}
		
				
		return sb.toString();
	}
	
	public Endpoint getEndpoint() {
		return endpoint;
	}

	public SubscribeType getType() {
		return type;
	}
	
	public Map<String, String> getResources() {
		return resources;
	}
	
	public List<String> getResourcesValues() {
		return resourcesValues;
	}
}
