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
package org.sentilo.platform.server.parser;

import java.io.IOException;

import org.apache.http.HttpStatus;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.exception.MessageNotReadableException;


public class CatalogParser extends PlatformJsonMessageConverter {
	
	private final Logger logger = LoggerFactory.getLogger(CatalogParser.class);
	
	public CatalogInputMessage parsePostRequest(SentiloRequest request) throws PlatformException{						
		String providerId = request.getResourcePart(0);
		CatalogInputMessage inputMessage = readInternal(request, CatalogInputMessage.class);
		inputMessage.setProviderId(providerId);		
		
		return inputMessage;
	}
	
	public CatalogInputMessage parsePutRequest(SentiloRequest request) throws PlatformException{						
		String providerId = request.getResourcePart(0);
		CatalogInputMessage inputMessage = readInternal(request, CatalogInputMessage.class);
		inputMessage.setProviderId(providerId);		
		
		return inputMessage;
	}
		
	
	public CatalogInputMessage parseGetRequest(SentiloRequest request) {		
		String entityId = request.getEntitySource();							
				
		return new CatalogInputMessage(entityId, request.getParameters());
	}
	
	public CatalogDeleteInputMessage parseDeleteRequest(SentiloRequest request) throws PlatformException{
		return parseDeleteRequest(request, false);
	}
	
	public CatalogDeleteInputMessage parseDeleteRequest(SentiloRequest request, boolean deleteSimulate) throws PlatformException{				
		String providerId = request.getResourcePart(0);
		CatalogDeleteInputMessage inputMessage = (deleteSimulate?(CatalogDeleteInputMessage)readInternal(request, CatalogDeleteInputMessage.class):new CatalogDeleteInputMessage());					
		inputMessage.setProviderId(providerId);
								
		return inputMessage;
	}
	
	protected CatalogInputMessage readInternal(SentiloRequest request, Class<? extends CatalogInputMessage> clazz) throws JsonConverterException{		
		try{
			String body = request.getBody();
			logger.debug("Message: {}", body);
			CatalogInputMessage message = (CatalogInputMessage)readInternal(clazz, body);
			message.setBody(body);
			return message;
		}catch (IOException ex) {
			throw new JsonConverterException("Could not write JSON: " + ex.getMessage(), ex, true);			
		}catch(MessageNotReadableException mne){
			throw new JsonConverterException("Could not write JSON: " + mne.getMessage(), mne, true);
		}
	}
	
	public void writeResponse(SentiloRequest request, SentiloResponse response, CatalogResponseMessage message) throws PlatformException{
		
		//Para no mostrar el codigo de retorno en la respuesta que se le da al cliente final, lo eliminamos de message
		message.setCode(null);
		
		try{
			writeInternal(message, response);
		}catch(IOException ex){
			throw new PlatformException(HttpStatus.SC_INTERNAL_SERVER_ERROR, ex);
		}				
	}
		
}
