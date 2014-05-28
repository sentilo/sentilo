/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.server.http;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import javax.annotation.PreDestroy;

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
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.handler.HandlerPath;
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

  private HandlerLocator handlerLocator;
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

  @Autowired
  @Qualifier("alarmHandler")
  private AbstractHandler alarmHandler;
  @Autowired
  @Qualifier("catalogHandler")
  private AbstractHandler catalogHandler;
  @Autowired
  @Qualifier("dataHandler")
  private AbstractHandler dataHandler;
  @Autowired
  @Qualifier("orderHandler")
  private AbstractHandler orderHandler;
  @Autowired
  @Qualifier("subscribeHandler")
  private AbstractHandler subscribeHandler;
  @Autowired
  @Qualifier("adminHandler")
  private AbstractHandler adminHandler;
  @Autowired
  @Qualifier("catalogAlertHandler")
  private AbstractHandler catalogAlertHandler;

  @Override
  public void run() {
    try {
      initialize();
      logger.info("Listening on port {}", port);
      while (notInterrupted()) {
        manageConnection(new DefaultHttpServerConnection());
      }
    } catch (final IOException ioe) {
      logger.error("Error while initializing connection thread. {}", ioe);
    } catch (final Throwable t) {
      logger.error("Error while running sentilo server on port {}. {}", port, t);
    } finally {
      cleanUp();
    }
  }

  @PreDestroy
  public void cleanUp() {
    stopThreadPool();
    releaseSocketPort();
  }

  private void initialize() throws IOException {
    logger.info("Initializing server");

    registerHandlers();
    initializeListener();
    initializeConnectionParams();
    registerURLS();
    initializeThreadPool();

    logger.info("Server is initialized.");
  }

  private void registerHandlers() {

    logger.info("Registering services");

    handlerLocator = new HandlerLocator();

    registerHandler(HandlerPath.ORDER, orderHandler);
    registerHandler(HandlerPath.SUBSCRIBE, subscribeHandler);
    registerHandler(HandlerPath.ALARM, alarmHandler);
    registerHandler(HandlerPath.CATALOG, catalogHandler);
    registerHandler(HandlerPath.DATA, dataHandler);
    registerHandler(HandlerPath.ADMIN, adminHandler);
    registerHandler(HandlerPath.CATALOG_ALERT, catalogAlertHandler);

    logger.info("Services registered");
  }

  private void registerHandler(final HandlerPath handler, final AbstractHandler handlerImpl) {
    logger.info("Registering {} handler", handler.toString());
    handlerLocator.register(handler, handlerImpl);
  }

  private void initializeListener() throws IOException {
    logger.info("Initializing listener on port {}", port);
    serverSocket = new ServerSocket(port);
  }

  private void initializeThreadPool() {
    threadPool.initialize();
  }

  private void initializeConnectionParams() {
    params = new SyncBasicHttpParams();
    params.setIntParameter(CoreConnectionPNames.SO_TIMEOUT, getSocketMillisecondsTimeout());
    params.setIntParameter(CoreConnectionPNames.SOCKET_BUFFER_SIZE, getSocketBufferSize());
    params.setBooleanParameter(CoreConnectionPNames.STALE_CONNECTION_CHECK, isStaleConnectionCheck());
    params.setBooleanParameter(CoreConnectionPNames.TCP_NODELAY, isTcpNoDelay());
    params.setParameter(CoreProtocolPNames.ORIGIN_SERVER, getOriginServer());
  }

  private void registerURLS() {
    final HttpProcessor httpproc =
        new ImmutableHttpProcessor(new HttpRequestInterceptor[] {new RequestExpectContinue()}, new HttpResponseInterceptor[] {new ResponseDate(),
            new ResponseServer(), new ResponseContent(), new ResponseConnControl()});
    final HttpRequestHandlerRegistry reqistry = new HttpRequestHandlerRegistry();
    reqistry.register(getRegisteredURLs(), new SentiloRequestHandler(handlerLocator, authenticationService));

    httpService = new HttpService(httpproc, new DefaultConnectionReuseStrategy(), new DefaultHttpResponseFactory(), reqistry, params);
  }

  private void manageConnection(final DefaultHttpServerConnection conn) throws IOException {
    final Socket s = serverSocket.accept();
    conn.bind(s, params);
    threadPool.submit(new SentiloThreadPoolExecutor(httpService, conn));
  }

  private boolean notInterrupted() {
    return !Thread.interrupted();
  }

  private void stopThreadPool() {
    try {
      if (threadPool != null) {
        threadPool.shutdown();
      }
    } catch (final Throwable e) {
      // ignore the error
    }

    logger.info("Thread pool shutdown");
  }

  private void releaseSocketPort() {
    try {
      if (!serverSocket.isClosed()) {
        serverSocket.close();
      }
    } catch (final IOException e) {
      // ignore the error
    }

    logger.info("Listener off on port {}", port);
  }

  public int getPort() {
    return port;
  }

  public void setPort(final int port) {
    this.port = port;
  }

  public int getSocketMillisecondsTimeout() {
    return socketMillisecondsTimeout;
  }

  public void setSocketMillisecondsTimeout(final int socketMillisecondsTimeout) {
    this.socketMillisecondsTimeout = socketMillisecondsTimeout;
  }

  public int getSocketBufferSize() {
    return socketBufferSize;
  }

  public void setSocketBufferSize(final int socketBufferSize) {
    this.socketBufferSize = socketBufferSize;
  }

  public boolean isStaleConnectionCheck() {
    return staleConnectionCheck;
  }

  public void setStaleConnectionCheck(final boolean staleConnectionCheck) {
    this.staleConnectionCheck = staleConnectionCheck;
  }

  public boolean isTcpNoDelay() {
    return tcpNoDelay;
  }

  public void setTcpNoDelay(final boolean tcpNoDelay) {
    this.tcpNoDelay = tcpNoDelay;
  }

  public String getOriginServer() {
    return originServer;
  }

  public void setOriginServer(final String originServer) {
    this.originServer = originServer;
  }

  public String getRegisteredURLs() {
    return registeredURLs;
  }

  public void setRegisteredURLs(final String registeredURLs) {
    this.registeredURLs = registeredURLs;
  }

  public void setThreadPool(final ThreadPool threadPool) {
    this.threadPool = threadPool;
  }

  public void setAlarmHandler(final AbstractHandler alarmHandler) {
    this.alarmHandler = alarmHandler;
  }

  public void setCatalogHandler(final AbstractHandler catalogHandler) {
    this.catalogHandler = catalogHandler;
  }

  public void setDataHandler(final AbstractHandler dataHandler) {
    this.dataHandler = dataHandler;
  }

  public void setOrderHandler(final AbstractHandler orderHandler) {
    this.orderHandler = orderHandler;
  }

  public void setSubscriptionHandler(final AbstractHandler subscriptionHandler) {
    subscribeHandler = subscriptionHandler;
  }

  public void setAuthenticationService(final AuthenticationService authenticationService) {
    this.authenticationService = authenticationService;
  }
}
