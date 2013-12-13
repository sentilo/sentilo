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
package org.sentilo.platform.service.impl;

import org.sentilo.common.domain.AlertsOwnersResponseMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.platform.common.domain.CredentialsMessage;
import org.sentilo.platform.common.domain.PermissionsMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.service.parser.CatalogServiceParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.NestedRuntimeException;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;


@Service
public class CatalogServiceImpl implements CatalogService {
	
	private final Logger logger = LoggerFactory.getLogger(CatalogServiceImpl.class);
	
	@Autowired
	private RESTClient restClient;
	
	private CatalogServiceParser parser = new CatalogServiceParser();
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#getPermissions()
	 */
	public PermissionsMessage getPermissions() {
		try{
			String response = restClient.get(buildApiPath("permissions"));
			return parser.parsePermissions(response);		
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#getCredentials()
	 */
	public CredentialsMessage getCredentials() {
		try{
			String response = restClient.get(buildApiPath("credentials"));
			return parser.parseCredentials(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
		
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#insertSensors(org.sentilo.common.domain.CatalogInputMessage)
	 */
	public CatalogResponseMessage insertSensors(CatalogInputMessage message) {
		try{
			String path = buildApiPath("provider", message.getProviderId());
			String response = restClient.post(path, message.getBody());			
			return parser.parseCatalogResponse(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#updateSensorsOrComponents(org.sentilo.common.domain.CatalogInputMessage)
	 */
	public CatalogResponseMessage updateSensorsOrComponents(CatalogInputMessage message) {
		try{
			String path = buildApiPath("provider", message.getProviderId()); 				
			String response = restClient.put(path, message.getBody());			
			return parser.parseCatalogResponse(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#getAuthorizedProviders(org.sentilo.common.domain.CatalogInputMessage)
	 */
	public CatalogResponseMessage getAuthorizedProviders(CatalogInputMessage message) {
		try{			
			RequestParameters parameters = new RequestParameters();
			String path = buildApiPath("authorized","provider",message.getEntityId()); 
				
			if(StringUtils.hasText(message.getSensorType())){
				parameters.put("type", message.getSensorType());
			}			
			String response = restClient.get(path, parameters);			
			return parser.parseCatalogResponse(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}	
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#getAlertsOwners()
	 */
	public AlertsOwnersResponseMessage getAlertsOwners(){
		try{
			String response = restClient.get(buildApiPath("alerts"));
			return parser.parseAlertsOwners(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.platform.common.service.CatalogService#deleteProvider(org.sentilo.common.domain.CatalogInputMessage)
	 */
	public CatalogResponseMessage deleteProvider(CatalogInputMessage message){
		try{
			String path = buildApiPath("delete","provider",message.getProviderId()); 								
			String response = restClient.put(path, message.getBody());						
			return parser.parseCatalogResponse(response);
		}catch(NestedRuntimeException rce){
			throw translateException(rce);
		}
	}
	
	private String buildApiPath(String ... pathTokens){
		StringBuilder sb = new StringBuilder("api");
		
		for(String pathToken:pathTokens){
			sb.append("/").append(pathToken);
		}
		
		return sb.toString();
	}
	
	private CatalogAccessException translateException(NestedRuntimeException nre){
		logger.debug("Translating exception of type {} ", nre.getClass());
		return new CatalogAccessException(nre.getMessage(), nre);
	}

	public void setRestClient(RESTClient restClient) {
		this.restClient = restClient;
	}

}
