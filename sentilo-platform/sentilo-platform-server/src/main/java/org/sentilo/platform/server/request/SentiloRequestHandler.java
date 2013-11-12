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
package org.sentilo.platform.server.request;

import org.apache.http.Header;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.util.EntityUtils;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.http.HttpHeader;
import org.sentilo.platform.server.response.SentiloResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SentiloRequestHandler implements HttpRequestHandler {

	private final Logger logger = LoggerFactory.getLogger(SentiloRequestHandler.class);
	
	private HandlerLocator serviceLocator;
	private AuthenticationService authenticationService;
	

	public SentiloRequestHandler(HandlerLocator serviceLocator, AuthenticationService authService) {
		this.serviceLocator = serviceLocator;
		this.authenticationService = authService;
	}

	@Override
	public void handle(HttpRequest httpRequest, HttpResponse httpResponse, HttpContext httpContext) {

		try {
			SentiloRequest request = SentiloRequest.build(httpRequest);
			SentiloResponse response = SentiloResponse.build(httpResponse);
			debug(request);
			request.checkCredentialIntegrity(authenticationService);

			AbstractHandler handler = lookupHandlerForRequest(request);
			handler.manageRequest(request, response);

			prepareResponse(httpResponse, request.getContentType().toString());
		} catch (PlatformException e) {
			prepareErrorResponse(httpResponse, e);
		}
		debug(httpResponse);
	}

	private void prepareErrorResponse(HttpResponse response, PlatformException e) {
		if (e.getHttpStatus() != 0) {
			response.setStatusCode(e.getHttpStatus());			
			// Actualizamos el motivo del error con la info de la excepcion en el body de la respuesta
			String reason = e.getMessage(); 
			response.setEntity(new ByteArrayEntity(reason.getBytes()));			
		} else {
			response.setStatusCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
		}
	}	

	private void prepareResponse(HttpResponse httpResponse, String contentType) {
		httpResponse.setStatusCode(HttpStatus.SC_OK);
		httpResponse.setHeader(HttpHeader.CONTENT_TYPE.toString(), contentType);
	}

	private AbstractHandler lookupHandlerForRequest(SentiloRequest request) throws PlatformException {		
		logger.debug("Looking handler for path {}" ,request.getHandlerPath());
		
		AbstractHandler service = serviceLocator.lookup(request.getHandlerPath());
		if (service == null) {
			throw new PlatformException(HttpStatus.SC_NOT_FOUND,	"service not found");
		}
		return service;
	}

	private void debug(SentiloRequest request) {
		if (logger.isDebugEnabled()) {
			logger.debug("New http {} request: {}", request.getMethod(),request.getUri());
			logger.debug("Content-Type: {}", request.getContentType());
		}
	}

	private void debug(HttpResponse httpResponse) {
		if (logger.isDebugEnabled()) {
			logger.debug("New http response:");

			if (httpResponse.getStatusLine() != null) {
				int status = httpResponse.getStatusLine().getStatusCode();
				logger.debug("Status: {}", status);
			}

			Header[] header = httpResponse.getHeaders(HttpHeader.CONTENT_TYPE.toString());
			if (header != null && header.length > 0) {
				String contentType = header[0].getValue();
				logger.debug("{} : {}", HttpHeader.CONTENT_TYPE.toString(),contentType);
			}

			if (httpResponse.getEntity() != null) {
				try {
					logger.debug("Entity body: {} ", EntityUtils.toString(httpResponse.getEntity()));
				} catch (Exception e) {
					logger.error("Error parsing body", e);
				}
			}
		}
	}
}
