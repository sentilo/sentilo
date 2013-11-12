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

import java.util.List;

import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.map.annotate.JsonSerialize;


public class CatalogInputMessage implements PlatformInputMessage{
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
	private String providerId;	
	@JsonIgnore
	private String sensorType;
	@JsonIgnore
	private String entityId;
	@JsonIgnore
	private String body;
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
	private List<CatalogSensor> sensors;	
	
	@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
	private List<CatalogComponent> components;
	
	public CatalogInputMessage(){
		super();
	}
	
	public CatalogInputMessage(String entityId, String sensorType){
		this();
		this.entityId = entityId;
		this.sensorType = sensorType;
	}
	
		
	public void setProviderId(String providerId) {
		this.providerId = providerId;
	}
	
	public String getProviderId() {
		return providerId;
	}

	public void setSensors(List<CatalogSensor> sensors) {
		this.sensors = sensors;
	}

	public List<CatalogSensor> getSensors() {
		return sensors;
	}


	public void setBody(String body) {
		this.body = body;
	}


	public String getBody() {
		return body;
	}


	public void setSensorType(String sensorType) {
		this.sensorType = sensorType;
	}


	public String getSensorType() {
		return sensorType;
	}


	public void setEntityId(String entityId) {
		this.entityId = entityId;
	}


	public String getEntityId() {
		return entityId;
	}

	public List<CatalogComponent> getComponents() {
		return components;
	}

	public void setComponents(List<CatalogComponent> components) {
		this.components = components;
	}
}
