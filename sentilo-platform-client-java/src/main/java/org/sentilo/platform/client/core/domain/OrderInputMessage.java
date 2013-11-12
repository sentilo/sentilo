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

import org.sentilo.common.domain.OrderMessage;
import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.platform.client.core.utils.ResourcesUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;


public class OrderInputMessage implements PlatformClientInputMessage, PlatformSearchInputMessage{
	
	private String providerId;
	private String sensorId;	
	
	private OrderMessage order;
	private String identityToken;
	
	private QueryFilterParams queryFilters;
	
	/** Lista ordenada de los identificadores que forman el path del recurso. */
	private final List<String> resourcesValues = new ArrayList<String>();	
	
	public OrderInputMessage(String providerId, String sensorId){
		super();
		Assert.isTrue(StringUtils.hasText(providerId));		
		
		this.providerId = providerId;		
		this.sensorId = sensorId;		
		
		ResourcesUtils.addToResources(providerId, getResourcesValues());
		ResourcesUtils.addToResources(sensorId, getResourcesValues());	
	}
	
	
	public OrderInputMessage(String providerId){
		this(providerId, (String)null);
	}
	
	public OrderInputMessage(String providerId, QueryFilterParams queryFilters){
		this(providerId);
		this.queryFilters = queryFilters;
	}
	
	
	
	public OrderInputMessage(String providerId, String sensorId, QueryFilterParams queryFilters){
		this(providerId, sensorId);
		this.queryFilters = queryFilters;
	}
	
	
	/**
	 * Constructor a utilizar cuando se quiere publicar una orden destinada a todos los sensores de un proveedor.
	 * @param providerId
	 * @param order
	 */
	public OrderInputMessage(String providerId, OrderMessage order){
		this(providerId, (String)null, order);		
	}
	
	/**
	 * Constructor a utilizar cuando se quiere publicar una orden destinada a un sensor conncreto de un proveedor.
	 * @param providerId
	 * @param sensorId
	 * @param order
	 */	
	public OrderInputMessage(String providerId, String sensorId, OrderMessage order){
		this(providerId, sensorId);
				
		Assert.notNull(order);
		Assert.isTrue(StringUtils.hasText(order.getOrder()));
				
		this.order = order;			
	}
	
	
	public String getIdentityToken() {
		return identityToken;
	}

	public void setIdentityToken(String identityToken) {
		this.identityToken = identityToken;
	}
	
	public String getProviderId() {
		return providerId;
	}
	
	public String getSensorId() {
		return sensorId;
	}
	
	public OrderMessage getOrder() {
		return order;
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
		sb.append("--- Order ---");
		sb.append("\n\t provider:" + providerId);
		if(StringUtils.hasText(sensorId)){
			sb.append("\n\t sensor:" + sensorId);
		}
		
		if(order!=null){
			sb.append("\n\t order:" + getOrder().getOrder());
		}
		
		if(hasQueryFilters()){
			sb.append(getQueryFilters());
		}
		
		return sb.toString();
	}		
}
