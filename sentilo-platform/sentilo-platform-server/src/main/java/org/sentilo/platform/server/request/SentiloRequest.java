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

import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpRequest;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.util.EntityUtils;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.exception.UnauthorizedException;
import org.sentilo.platform.server.http.ContentType;
import org.sentilo.platform.server.http.HttpHeader;
import org.sentilo.platform.server.http.HttpMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.Assert;


public class SentiloRequest {

	private final Logger logger = LoggerFactory.getLogger(SentiloRequestHandler.class);
	private static final String UTF8 = "UTF-8";
	
	private HttpRequest httpRequest;
	
	private RequestParameters parameters;
	private SentiloResource resource;
	/** Token del path de la peticion que identifica el servicio invocado.*/
	private String handlerPath;
	private String uri;
	private URI parsedUri;
	private String entitySource;
	private ContentType contentType;
	private HttpMethod method;

	public static SentiloRequest build(HttpRequest httpRequest) {
		Assert.notNull(httpRequest);
		SentiloRequest request = new SentiloRequest();
		request.parseHttpRequest(httpRequest);
		return request;
	}
	
	public void checkCredentialIntegrity(AuthenticationService authenticationService) throws UnauthorizedException{
		String credential = extractHeader(HttpHeader.IDENTITY_KEY);
		entitySource = authenticationService.getIdentity(credential);
	}
	

	public String getResourcePart(int pos){
		return resource.getResourcePart(pos);
	}
	
	public String getRequestParameter(String paramName) {		
		return parameters.get(paramName);
	}
	
	public Map<String,String> getParameters(){
		return parameters.getParameters();
	}

	public void processResource(String path) {
		resource = new SentiloResource(RequestUtils.extractResource(path));
	}
	
	public String getHandlerPath() {
		return handlerPath;
	}	

	public SentiloResource getResource() {
		return resource;
	}		

	public String getUri() {
		return uri;
	}

	public String getBody() throws IOException {				
		HttpEntity entity = ((HttpEntityEnclosingRequest) httpRequest).getEntity();			
		debug(entity);			
		return EntityUtils.toString(entity, UTF8);					
	}

	public ContentType getContentType() {
		return contentType;
	}

	public String getEntitySource() {
		return entitySource;
	}	

	private ContentType getDefaultContentType() {
		return ContentType.JSON;
	}

	public HttpMethod getMethod() {
		return method;
	}

	private void debug(HttpEntity entity) {
		if (logger.isDebugEnabled()) {
			logger.debug("Default charset: {}", Charset.defaultCharset());
			logger.debug("Entity Content-Type: {}", org.apache.http.entity.ContentType.getOrDefault(entity));
		}
	}

	private void parseHttpRequest(HttpRequest request) {
		httpRequest = request;		
		parseMethod();
		parseContentType();
		parseUri();
	}

	private void parseMethod() {
		method = HttpMethod.valueOf(httpRequest.getRequestLine().getMethod());
	}

	private void parseContentType() {
		contentType = ContentType.fromString(extractHeader(HttpHeader.CONTENT_TYPE));
		logger.debug("Parsed Content-type: {}", contentType);
		if (contentType == null) {
			contentType = getDefaultContentType();
		}
	}	

	private void parseUri() {
		uri = httpRequest.getRequestLine().getUri();
		parsedUri = URI.create(uri);
		String path = parsedUri.getPath();
		handlerPath = RequestUtils.extractHandlerPath(path);
		processUriParameters();
		processResource(path);
	}

	private void processUriParameters() {
		List<NameValuePair> pairs = URLEncodedUtils.parse(parsedUri, UTF8);
		parameters = new RequestParameters(pairs);
	}

	private String extractHeader(HttpHeader header) {
		logger.debug("extractHeader: {}", header.toString());

		Header[] requestHeaders = httpRequest.getHeaders(header.toString());
		if (requestHeaders == null || requestHeaders.length == 0) {
			return null;
		}
		return requestHeaders[0].getValue();
	}
		
	
	public String toString() {
		StringBuilder sb = new StringBuilder();		
		sb.append("\n\t Entity source:  " +this.entitySource);
		sb.append("\n\t Service: " + this.handlerPath);
		if (this.resource != null) {
			sb.append("\n\t Resource: "+ this.resource.toString());
		}
		
		return sb.toString();
	}
}
