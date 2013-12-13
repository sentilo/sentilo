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
package org.sentilo.web.catalog.converter;

import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.SensorService;

public class ApiConverterContext {
	
	private CatalogInputMessage message;
	private SensorService sensorService;
	private ComponentService componentService;
	private String providerId;
	private boolean isUpdateAction;
	
	public ApiConverterContext(CatalogInputMessage message, SensorService sensorService, ComponentService componentService, String providerId) {
		super();
		this.message = message;
		this.sensorService = sensorService;
		this.componentService = componentService;
		this.providerId = providerId;
	}
			
	public ApiConverterContext(CatalogInputMessage message, SensorService sensorService, ComponentService componentService, String providerId, boolean isUpdateAction) {
		this(message, sensorService, componentService, providerId);
		this.isUpdateAction = isUpdateAction;
	}

	public CatalogInputMessage getMessage() {
		return message;
	}			
	public void setMessage(CatalogInputMessage message) {
		this.message = message;
	}
	public SensorService getSensorService() {
		return sensorService;
	}
	public void setSensorService(SensorService sensorService) {
		this.sensorService = sensorService;
	}
	public ComponentService getComponentService() {
		return componentService;
	}
	public void setComponentService(ComponentService componentService) {
		this.componentService = componentService;
	}
	public String getProviderId() {
		return providerId;
	}
	public void setProviderId(String providerId) {
		this.providerId = providerId;
	}
	public boolean isUpdateAction() {
		return isUpdateAction;
	}
	public void setUpdateAction(boolean isUpdateAction) {
		this.isUpdateAction = isUpdateAction;
	}
	
	
}
