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
import org.springframework.util.StringUtils;


public class DataInputMessage implements PlatformClientInputMessage, PlatformSearchInputMessage{
	
	private String providerId;
	private String sensorId;
	private String observation;
	/** Listado de observaciones de un sensor.*/
	private SensorObservations sensorObservations;
	/** Listado de observaciones de N sensores diferentes de un proveedor.*/
	private ProviderObservations providerObservations;
	
	private QueryFilterParams queryFilters;
	
	private String identityToken;
	
	/** Lista ordenada de los identificadores que forman el path del recurso. */
	private final List<String> resourcesValues = new ArrayList<String>();
	
	public DataInputMessage(String providerId, String sensorId) {
		super();
		this.providerId = providerId;
		this.sensorId = sensorId;
		
		ResourcesUtils.addToResources(this.providerId, getResourcesValues());
		ResourcesUtils.addToResources(this.sensorId, getResourcesValues());						
	}
		
	public DataInputMessage(String providerId) {
		this(providerId, (String)null);
	}		
			
	public DataInputMessage(String providerId, String sensorId, String value) {
		this(providerId, sensorId);
		
		this.observation = value;				
		ResourcesUtils.addToResources(this.observation, getResourcesValues());
	}

	public DataInputMessage(String providerId, String sensorId, QueryFilterParams queryFilters) {
		this(providerId, sensorId);		
		this.queryFilters = queryFilters;
	}
	
	public DataInputMessage(String providerId, QueryFilterParams queryFilters) {
		this(providerId, (String)null, queryFilters);
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
		sb.append("--- Message ---");
		sb.append("\n\t provider:" + providerId);
		sb.append("\n\t sensor:" + sensorId);		
		if(hasQueryFilters()){
			sb.append(getQueryFilters());
		}
		sb.append("\n\t Observations");
		if(StringUtils.hasText(observation)){
			sb.append("\n\t observation" + observation);
		}
		if(sensorObservations!=null){
			sb.append(sensorObservations.toString());
		}
		if(providerObservations!=null){
			sb.append(providerObservations.toString());
		}
				
		return sb.toString();
	}
	
	public String getIdentityToken() {
		return identityToken;
	}

	public void setIdentityToken(String identityToken) {
		this.identityToken = identityToken;
	}
	
	public List<String> getResourcesValues() {
		return resourcesValues;
	}

	public String getProviderId() {
		return providerId;
	}

	public String getSensorId() {
		return sensorId;
	}
	
	public String getObservation() {
		return observation;
	}	

	public SensorObservations getSensorObservations() {
		return sensorObservations;
	}

	public void setSensorObservations(SensorObservations sensorObservations) {
		this.sensorObservations = sensorObservations;
	}

	public ProviderObservations getProviderObservations() {
		return providerObservations;
	}

	public void setProviderObservations(ProviderObservations providerObservations) {
		this.providerObservations = providerObservations;
	}		
}
