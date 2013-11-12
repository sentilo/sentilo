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
package org.sentilo.platform.server.handler;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.auth.AuthorizationService;
import org.sentilo.platform.server.exception.ForbiddenAccessException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;


public abstract class AbstractHandler {

	private final Logger logger = LoggerFactory.getLogger(AbstractHandler.class);
	
	@Autowired
	protected AuthorizationService authorizationService;

	public abstract void onDelete(SentiloRequest request, SentiloResponse response) throws PlatformException;

	public abstract void onGet(SentiloRequest request, SentiloResponse response) throws PlatformException;

	public abstract void onPost(SentiloRequest request, SentiloResponse response) throws PlatformException;

	public abstract void onPut(SentiloRequest request, SentiloResponse response) throws PlatformException;

	
	public final void manageRequest(SentiloRequest request, SentiloResponse response) throws PlatformException {
		
		logger.debug("Manage {} request", request.getMethod());		
		
		switch (request.getMethod()) {
			case DELETE:
				onDelete(request, response);
				break;
			case GET:
				onGet(request, response);
				break;
			case POST:
				onPost(request, response);
				break;
			case PUT:
				onPut(request, response);
				break;
		}
	}
	
	protected void validateReadAccess(String source, String target) throws ForbiddenAccessException{
		if(!authorizationService.hasAccessToRead(source, target)){
			throw new ForbiddenAccessException(source,target,"READ");
		}
	}
	
	protected void validateWriteAccess(String source, String target) throws ForbiddenAccessException{
		if(!authorizationService.hasAccessToWrite(source, target)){
			throw new ForbiddenAccessException(source,target,"WRITE");
		}
	}
	
	protected void validateResourceNumberParts(SentiloRequest request, int min, int max) throws PlatformException{
		// Validamos que el path tenga el numero de tokens que corresponde a la accion invocada
		if(!numberArgumentsValid(request.getResource().getParts(), min, max)){
			throw new PlatformException(HttpStatus.SC_BAD_REQUEST, "Invalid path was requested:"+request.getResource().getPath());
		}
	}
	
	protected boolean numberArgumentsValid(String[] arguments, int min, int max){
		return  (arguments==null? min==0:(arguments.length>=min && arguments.length<=max));
	}
	

	protected void debug(SentiloRequest request) {		
		logger.debug(request.toString());				
	}

	public void setAuthorizationService(AuthorizationService authorizationService) {
		this.authorizationService = authorizationService;
	}
}
