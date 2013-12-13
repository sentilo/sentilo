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
package org.sentilo.common.rest.impl;

import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.Credentials;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.PoolingClientConnectionManager;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.RESTUtils;
import org.sentilo.common.utils.URIUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.StringUtils;


public class RESTClientImpl implements RESTClient, InitializingBean {

	private final Logger logger = LoggerFactory.getLogger(RESTClientImpl.class);	
			
	private static final int DEFAULT_CONNECTION_TIMEOUT_MILLISECONDS = (60 * 1000);
    private static final int DEFAULT_READ_TIMEOUT_MILLISECONDS = (60 * 1000);
    
    private HttpClient httpClient;
    private Credentials credentials;    
    private AuthScope authScope = AuthScope.ANY;
    private HttpRequestInterceptor[] interceptors;
    
    
    private String host; 
     
	public RESTClientImpl() {
	}

	public String get(String path) throws RESTClientException {
		return get(path, (String)null);
	}
	
	public String get(String path, String identityToken) throws RESTClientException {
		return get(path, (RequestParameters)null, identityToken);
	}

	public String get(String path, RequestParameters parameters) throws RESTClientException {
		return get(path, parameters, (String)null);								
	}
	
	public String get(String path, RequestParameters parameters, String identityToken) throws RESTClientException {

		String url = URIUtils.getURI(host, path, parameters);
		HttpGet get = new HttpGet(url);	
				
		return executeHttpCall(get, identityToken);						
	}

	public String post(String path, String body) throws RESTClientException {				
		return post(path, body, null);	
	}
	
	public String post(String path, String body, String identityToken) throws RESTClientException {
		logger.debug("Send post message to host {} and path {}", host, path);
		String url = URIUtils.getURI(host, path);
		HttpPost post = new HttpPost(url);
		logger.debug("Token {}", identityToken);
		logger.debug("Body {}", body);		
		return executeHttpCall(post, body, identityToken);	
	}

	public String put(String path, String body) throws RESTClientException {		
		return put(path, body, null);				
	}
	
	public String put(String path, String body, String identityToken) throws RESTClientException {
		String url = URIUtils.getURI(host, path);
		HttpPut put = new HttpPut(url);
		
		return executeHttpCall(put, body, identityToken);				
	}

	public String delete(String path) throws RESTClientException {
		return delete(path, null);		
	}
	
	public String delete(String path, String identityToken) throws RESTClientException {				
		return delete(path, identityToken, null);		
	}
	
	public String delete(String path, String body, String identityToken) throws RESTClientException {
		
		// Una peticion DELETE no puede tener body por lo que en caso de que venga informado el parametro body,
		// simulamos la llamada al DELETE haciendo un PUT con el parametro method=delete		
		String url = (StringUtils.hasText(body)?URIUtils.getURI(host, path, RequestParameters.buildDelete()):URIUtils.getURI(host, path));		
		HttpRequestBase delete = (StringUtils.hasText(body)?new HttpPut(url):new HttpDelete(url));		
				
		return executeHttpCall(delete, body, identityToken);	
	}
	
	@Override
	public void afterPropertiesSet() throws Exception {
		if(httpClient == null){
			httpClient = new DefaultHttpClient(new PoolingClientConnectionManager());
			// Fijamos los timeouts de establecimiento de conexion y de lectura de respuesta
			setConnectionTimeout(DEFAULT_CONNECTION_TIMEOUT_MILLISECONDS);
			setReadTimeout(DEFAULT_READ_TIMEOUT_MILLISECONDS);
		}
		
		if(interceptors!=null && httpClient instanceof DefaultHttpClient){
			for(HttpRequestInterceptor interceptor: interceptors){
				((DefaultHttpClient)httpClient).addRequestInterceptor(interceptor);
			}
		}
		
		if (credentials != null && httpClient instanceof DefaultHttpClient) {
            ((DefaultHttpClient) httpClient).getCredentialsProvider().setCredentials(authScope, credentials);
                      
        }
	}
		
	public void destroy() throws Exception {
	    // Tal y como recomienda la API de HttpClient, al destruir la clase cliente cerramos el connectionManager asociado a la
		// clase HttpClient.
		this.httpClient.getConnectionManager().shutdown();
	}			

	private void validateResponse(HttpResponse response) throws RESTClientException {
		if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {																				
			StatusLine line = response.getStatusLine();
			StringBuilder sb = new StringBuilder(line.getReasonPhrase());
			try {
				if(response.getEntity()!=null){
					sb.append(EntityUtils.toString(response.getEntity())+". ");
				}
			} catch (Exception e) {
				//Ignored
			}
			throw new RESTClientException(line.getStatusCode(),	sb.toString());																
		}
	}
	
	private String executeHttpCall(HttpRequestBase httpRequest, String identityToken) throws RESTClientException{
		return executeHttpCall(httpRequest, null, identityToken);
	}

	private String executeHttpCall(HttpRequestBase httpRequest, String body, String identityToken) throws RESTClientException{				
		try {
			logger.debug("Executing http call {} ", httpRequest.toString());
			if(StringUtils.hasText(body)){
				((HttpEntityEnclosingRequestBase)httpRequest).setEntity(new StringEntity(body, ContentType.APPLICATION_JSON));
			}
			
			if(StringUtils.hasText(identityToken)){
				httpRequest.addHeader(RESTUtils.buildIdentityHeader(identityToken));
			}			
			HttpResponse response = httpClient.execute(httpRequest);
			validateResponse(response);
			return EntityUtils.toString(response.getEntity());
		} catch (RESTClientException e) {
			throw e;
		} catch (Exception e) {
			String msg = String.format("Error while executing http call: %s ", httpRequest.toString());
			throw new RESTClientException(msg, e);
		} 
	}	
			
	/**
     * Sets the timeout until a connection is established. A value of 0 means <em>never</em> timeout.
     *
     * @param timeout the timeout value in milliseconds
     * @see org.apache.http.params.HttpConnectionParams#setConnectionTimeout(org.apache.http.params.HttpParams, int)
     */
    public void setConnectionTimeout(int timeout) {
        if (timeout < 0) {
            throw new IllegalArgumentException("timeout must be a non-negative value");
        }
        HttpConnectionParams.setConnectionTimeout(httpClient.getParams(), timeout);
    }

    /**
     * Set the socket read timeout for the underlying HttpClient. A value of 0 means <em>never</em> timeout.
     *
     * @param timeout the timeout value in milliseconds
     * @see org.apache.http.params.HttpConnectionParams#setSoTimeout(org.apache.http.params.HttpParams, int)
     */
    public void setReadTimeout(int timeout) {
        if (timeout < 0) {
            throw new IllegalArgumentException("timeout must be a non-negative value");
        }
        HttpConnectionParams.setSoTimeout(httpClient.getParams(), timeout);
    }        
	
	public void setHost(String host) {
		this.host = host;
	}	

	public void setHttpClient(HttpClient httpClient) {
		this.httpClient = httpClient;
	}		

	public void setInterceptors(HttpRequestInterceptor[] interceptors) {
		this.interceptors = interceptors;
	}

	public void setCredentials(Credentials credentials) {
		this.credentials = credentials;
	}	
}
