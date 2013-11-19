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
package org.sentilo.platform.server.http;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.apache.http.HttpRequestInterceptor;
import org.apache.http.HttpResponseInterceptor;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.impl.DefaultHttpResponseFactory;
import org.apache.http.impl.DefaultHttpServerConnection;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.params.HttpParams;
import org.apache.http.params.SyncBasicHttpParams;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpRequestHandlerRegistry;
import org.apache.http.protocol.HttpService;
import org.apache.http.protocol.ImmutableHttpProcessor;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.ResponseConnControl;
import org.apache.http.protocol.ResponseContent;
import org.apache.http.protocol.ResponseDate;
import org.apache.http.protocol.ResponseServer;
import org.sentilo.platform.server.SentiloThreadPoolExecutor;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.Handler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.pool.ThreadPool;
import org.sentilo.platform.server.request.SentiloRequestHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;


public class RequestListenerThread extends Thread {

	private final Logger logger = LoggerFactory.getLogger(RequestListenerThread.class);

	private ServerSocket serverSocket;
	private HttpParams params;
	private HttpService httpService;

	private HandlerLocator serviceLocator;
	@Autowired
	private AuthenticationService authenticationService;

	@Autowired
	private ThreadPool threadPool;

	private int port;
	private int socketMillisecondsTimeout;
	private int socketBufferSize;
	private boolean staleConnectionCheck;
	private boolean tcpNoDelay;
	private String originServer;
	private String registeredURLs;

	@Autowired @Qualifier("alarmHandler")
	private AbstractHandler alarmHandler;
	@Autowired @Qualifier("catalogHandler")
	private AbstractHandler catalogHandler;
	@Autowired @Qualifier("dataHandler")
	private AbstractHandler dataHandler;
	@Autowired @Qualifier("orderHandler")
	private AbstractHandler orderHandler;
	@Autowired @Qualifier("subscribeHandler")
	private AbstractHandler subscribeHandler;
	@Autowired @Qualifier("adminHandler")
	private AbstractHandler adminHandler;

	@Override
	public void run() {
		try {
			initialize();
			logger.info("Listening on port {}", port);
			while (notInterrupted()) {
				manageConnection(new DefaultHttpServerConnection());
			}
		} catch (IOException ioe) {
			logger.error("Error while initializing connection thread. {}", ioe);
		} catch (Throwable t){
			logger.error("Error while running sentilo server on port {}. {}", port, t);			
		}finally {
			stopThreadPool();
			releaseSocketPort();
		}
	}	

	private void initialize() throws IOException {
		logger.info("Initializing server");

		registerServices();
		initializeListener();
		initializeConnectionParams();
		registerURLS();
		initializeThreadPool();

		logger.info("Server is initialized.");
	}

	private void registerServices() {

		logger.info("Registering services");
		
		serviceLocator = new HandlerLocator();

		registerService(Handler.ORDER, orderHandler);
		registerService(Handler.SUBSCRIBE, subscribeHandler);
		registerService(Handler.ALARM, alarmHandler);
		registerService(Handler.CATALOG, catalogHandler);
		registerService(Handler.DATA, dataHandler);
		registerService(Handler.ADMIN, adminHandler);

		logger.info("Services registered");
	}

	private void registerService(Handler service, AbstractHandler impl) {
		logger.info("Registering {} service", service.toString());
		serviceLocator.register(service, impl);
	}

	private void initializeListener() throws IOException {
		logger.info("Initializing listener on port {}",port);
		this.serverSocket = new ServerSocket(port);
	}

	private void initializeThreadPool() {
		threadPool.initialize();
	}

	private void initializeConnectionParams() {
		params = new SyncBasicHttpParams();
		params.setIntParameter(CoreConnectionPNames.SO_TIMEOUT,	getSocketMillisecondsTimeout());
		params.setIntParameter(CoreConnectionPNames.SOCKET_BUFFER_SIZE, getSocketBufferSize());
		params.setBooleanParameter(CoreConnectionPNames.STALE_CONNECTION_CHECK, isStaleConnectionCheck());
		params.setBooleanParameter(CoreConnectionPNames.TCP_NODELAY, isTcpNoDelay());
		params.setParameter(CoreProtocolPNames.ORIGIN_SERVER, getOriginServer());
	}

	private void registerURLS() {
		HttpProcessor httpproc = new ImmutableHttpProcessor(
				new HttpRequestInterceptor[] { new RequestExpectContinue() },
				new HttpResponseInterceptor[] { new ResponseDate(),
						new ResponseServer(), new ResponseContent(),
						new ResponseConnControl() });
		HttpRequestHandlerRegistry reqistry = new HttpRequestHandlerRegistry();
		reqistry.register(getRegisteredURLs(), new SentiloRequestHandler(serviceLocator, authenticationService));

		httpService = new HttpService(httpproc,
				new DefaultConnectionReuseStrategy(),
				new DefaultHttpResponseFactory(), reqistry, this.params);
	}

	private void manageConnection(DefaultHttpServerConnection conn)	throws IOException {
		Socket s = this.serverSocket.accept();
		conn.bind(s, params);
		threadPool.submit(new SentiloThreadPoolExecutor(httpService, conn));
	}

	private boolean notInterrupted() {
		return !Thread.interrupted();
	}

	private void stopThreadPool() {
		try{
			if (this.threadPool != null) {
				this.threadPool.shutdown();
			}
		}catch(Throwable e){
			//ignore the error
		}
		
		logger.info("Thread pool shutdown");	
	}
	
	private void releaseSocketPort() {
		try {
			if (this.serverSocket.isClosed() == false) {
				this.serverSocket.close();
			}
		} catch (IOException e) {
			//ignore the error
		}		
		
		logger.info("Listener off on port {}", port);				
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public int getSocketMillisecondsTimeout() {
		return socketMillisecondsTimeout;
	}

	public void setSocketMillisecondsTimeout(int socketMillisecondsTimeout) {
		this.socketMillisecondsTimeout = socketMillisecondsTimeout;
	}

	public int getSocketBufferSize() {
		return socketBufferSize;
	}

	public void setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public boolean isStaleConnectionCheck() {
		return staleConnectionCheck;
	}

	public void setStaleConnectionCheck(boolean staleConnectionCheck) {
		this.staleConnectionCheck = staleConnectionCheck;
	}

	public boolean isTcpNoDelay() {
		return tcpNoDelay;
	}

	public void setTcpNoDelay(boolean tcpNoDelay) {
		this.tcpNoDelay = tcpNoDelay;
	}

	public String getOriginServer() {
		return originServer;
	}

	public void setOriginServer(String originServer) {
		this.originServer = originServer;
	}

	public String getRegisteredURLs() {
		return registeredURLs;
	}

	public void setRegisteredURLs(String registeredURLs) {
		this.registeredURLs = registeredURLs;
	}
	
	public void setThreadPool(ThreadPool threadPool) {
		this.threadPool = threadPool;
	}
	

	public void setAlarmHandler(AbstractHandler alarmHandler) {
		this.alarmHandler = alarmHandler;
	}	

	public void setCatalogHandler(AbstractHandler catalogHandler) {
		this.catalogHandler = catalogHandler;
	}
	
	public void setDataHandler(AbstractHandler dataHandler) {
		this.dataHandler = dataHandler;
	}	

	public void setOrderHandler(AbstractHandler orderHandler) {
		this.orderHandler = orderHandler;
	}	

	public void setSubscriptionHandler(AbstractHandler subscriptionHandler) {
		this.subscribeHandler = subscriptionHandler;
	}

	public void setAuthenticationService(AuthenticationService authenticationService) {
		this.authenticationService = authenticationService;
	}	
}
