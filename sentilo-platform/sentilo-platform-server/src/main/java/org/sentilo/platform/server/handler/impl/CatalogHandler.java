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
package org.sentilo.platform.server.handler.impl;

import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.service.CatalogService;
import org.sentilo.platform.server.exception.CatalogErrorException;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.parser.CatalogParser;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.sentilo.platform.server.validation.CatalogValidator;
import org.sentilo.platform.server.validation.RequestMessageValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;

import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;

@Controller
public class CatalogHandler extends AbstractHandler {

	private final Logger logger = LoggerFactory.getLogger(CatalogHandler.class);
	
	@Autowired
	private CatalogService catalogService;
	
	private CatalogParser parser = new CatalogParser();
	private RequestMessageValidator<CatalogInputMessage> validator = new CatalogValidator();

	@Override
	public void onDelete(SentiloRequest request, SentiloResponse response) throws PlatformException {
		logger.debug("Executing catalog DELETE request");
		debug(request);
				
		_onDelete(request, response, false);
	}
	
	@Override
	public void onGet(SentiloRequest request, SentiloResponse response) throws PlatformException {
		logger.debug("Executing catalog GET request");		
		debug(request);
		
		//La peticion sólo puede ser de la siguiente manera
		// GET /catalog
		//Ademas, puede haber parametros en la URL
		
		validateResourceNumberParts(request, 0, 0);
		CatalogInputMessage inputMessage = parser.parseGetRequest(request);
		validator.validateRequestMessageOnGet(inputMessage);	
		//Aqui no tiene sentido hacer ninguna validación de autorizacion ya que se aplica sobre la misma entidad que hace la peticion
		CatalogResponseMessage responseMessage =  catalogService.getAuthorizedProviders(inputMessage);
		if(!responseMessage.getCode().equals(CatalogResponseMessage.OK)){
			throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage());
		}
		
		parser.writeResponse(request, response, responseMessage);
		
	}

	@Override
	public void onPost(SentiloRequest request, SentiloResponse response) 	throws PlatformException {
		logger.debug("Executing catalog POST request");
		debug(request);
		
		//La peticion sólo puede ser de la sigiente manera
		// POST /catalog/provider_id							
		validateResourceNumberParts(request, 1, 1);
		CatalogInputMessage inputMessage = parser.parsePostRequest(request);
		validator.validateRequestMessageOnPost(inputMessage);	
		validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());
		//Redirigir peticion al catálogo --> Cliente REST para el catálogo.		
		CatalogResponseMessage responseMessage =  catalogService.insertSensors(inputMessage);
		if(!responseMessage.getCode().equals(CatalogResponseMessage.OK)){
			throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage());
		}
		
	}

	@Override
	public void onPut(SentiloRequest request, SentiloResponse response) throws PlatformException {
		// Tanto las peticiones de actualizacion como las de borrado pueden acabar recibiendose via una llamada HTTP PUT
		// ya que el metodo HTTP DELETE no es implementado/soportado por todos los clientes  o bien no soportan
		// el envío de un body junto a la petición.
		// En estos casos, utilizaremos el atributo method=delete para indicar que se está siumulando una llamada DELETE
		logger.debug("Executing catalog PUT request");		
		debug(request);
		String method = request.getRequestParameter("method");
		if(StringUtils.hasText(method) && method.equals("delete")){
			_onDelete(request, response, true);
		}else{
			_onPut(request, response);
		}
	}
	
	private void _onDelete(SentiloRequest request, SentiloResponse response, boolean simulate) throws PlatformException {
		logger.debug("Executing catalog DELETE request");
		debug(request);
				
		//La peticion sólo puede ser de la sigiente manera
		// DELETE /catalog/provider_id
		
		validateResourceNumberParts(request, 1, 1);
		CatalogDeleteInputMessage inputMessage = parser.parseDeleteRequest(request, simulate);
		validator.validateRequestMessageOnDelete(inputMessage);			
		validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());
		
		CatalogResponseMessage responseMessage =  catalogService.deleteProvider(inputMessage);
		if(!responseMessage.getCode().equals(CatalogResponseMessage.OK)){
			throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage());
		}				
	} 
	
	
	public void _onPut(SentiloRequest request, SentiloResponse response) 	throws PlatformException {
		logger.debug("Executing catalog PUT request");
		debug(request);
		
		//La peticion sólo puede ser de la sigiente manera
		// PUT /catalog/provider_id	
		// CREAR TESTS DE TODO LO QUE SE HAGA!!!!!
		validateResourceNumberParts(request, 1, 1);
		CatalogInputMessage inputMessage = parser.parsePutRequest(request);
		validator.validateRequestMessageOnPut(inputMessage);	
		validateWriteAccess(request.getEntitySource(), inputMessage.getProviderId());
		//Redirigir peticion al catálogo --> Cliente REST para el catálogo.		
		CatalogResponseMessage responseMessage =  catalogService.updateSensorsOrComponents(inputMessage);
		if(!responseMessage.getCode().equals(CatalogResponseMessage.OK)){
			throw new CatalogErrorException(responseMessage.getCode(), responseMessage.getErrorMessage());
		}		
	}
			

	public void setCatalogService(CatalogService catalogService) {
		this.catalogService = catalogService;
	}

	public void setCatalogParser(CatalogParser parser) {
		this.parser = parser;
	}
}
