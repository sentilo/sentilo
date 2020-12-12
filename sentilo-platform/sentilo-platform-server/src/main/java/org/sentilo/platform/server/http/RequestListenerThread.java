/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
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
import java.net.SocketException;
import java.util.concurrent.locks.ReentrantLock;

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
import org.sentilo.platform.server.SentiloHttpRequestTask;
import org.sentilo.platform.server.pool.ThreadPool;
import org.sentilo.platform.server.request.SentiloRequestHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class RequestListenerThread extends Thread {

  private static final Logger LOGGER = LoggerFactory.getLogger(RequestListenerThread.class);
  private static final String THREAD_NAME = "API-Server-Thread";

  private ServerSocket serverSocket;
  private HttpParams params;
  private HttpService httpService;

  @Autowired()
  @Qualifier("apiServerRequestHandler")
  private SentiloRequestHandler requestHandler;

  @Autowired
  private ThreadPool threadPool;

  private int port;
  private int socketTcpBacklog;
  private int socketMillisecondsTimeout;
  private int socketBufferSize;
  private boolean staleConnectionCheck;
  private boolean tcpNoDelay;
  private String originServer;
  private String registeredURLs;

  private volatile boolean listening;
  private final ReentrantLock lock = new ReentrantLock();

  @Override
  public void run() {
    try {
      initialize();
      initConnectionDependencies();
      listening = true;

      while (!Thread.interrupted()) {
        manageConnection(new DefaultHttpServerConnection());
      }

    } catch (final IOException ioe) {
      LOGGER.error("Error while initializing connection thread. {}", ioe);
    } catch (final Exception e) {
      LOGGER.error("Error while running sentilo server on port {}. {}", port, e);
    } finally {
      listening = false;
      cleanUp();
    }
  }

  @PreDestroy
  public void cleanUp() {
    stopThreadPool();
    releaseSocketPort();
  }

  public boolean isListening() {
    return listening;
  }

  public void restart(final boolean force) {
    lock.lock();
    try {
      listening = false;

      if (isServerListening()) {
        if (!force) {
          // wait here until all current requests complete.
          LOGGER.info("Current requests number: {}", threadPool.getCurrentTasks());
          threadPool.shutdownControlled();
        }
        cleanUp();
      }

      initConnectionDependencies();
      listening = true;
    } catch (final IOException ioe) {
      LOGGER.error("Error while restarting API server. {}", ioe);
    } finally {
      lock.unlock();
    }

  }

  private void initialize() {
    LOGGER.info("Initializing api server");
    setName(THREAD_NAME);
    initializeConnectionParams();
    registerURLS();
  }

  private void initConnectionDependencies() throws IOException {
    LOGGER.info("Initializing connection dependencies");

    initializeListener();
    initializeThreadPool();
    LOGGER.info("Server initialized and listening on port {}", port);
  }

  private boolean isServerListening() {
    return serverSocket != null || threadPool != null;
  }

  private void initializeListener() throws IOException {
    LOGGER.info("Initializing listener on port {} with TCP backlog {}", port, getSocketTcpBacklog());
    serverSocket = new ServerSocket(port, getSocketTcpBacklog());
    /*
     * if (socketMillisecondsTimeout > 0) { serverSocket.setSoTimeout(socketMillisecondsTimeout); }
     */
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
    params.setBooleanParameter(CoreConnectionPNames.SO_KEEPALIVE, true);
  }

  private void registerURLS() {
    final HttpProcessor httpproc = new ImmutableHttpProcessor(new HttpRequestInterceptor[] {new RequestExpectContinue()},
        new HttpResponseInterceptor[] {new ResponseDate(), new ResponseServer(), new ResponseContent(), new ResponseConnControl()});
    final HttpRequestHandlerRegistry reqistry = new HttpRequestHandlerRegistry();
    reqistry.register(getRegisteredURLs(), requestHandler);

    httpService = new HttpService(httpproc, new DefaultConnectionReuseStrategy(), new DefaultHttpResponseFactory(), reqistry, params);
  }

  private void manageConnection(final DefaultHttpServerConnection conn) throws IOException, InterruptedException {

    try {
      if (lock.isLocked()) {
        LOGGER.warn("Server is restarting. No more requests accepted, wait for a seconds until server is up");
        sleep(500);
        return;
      }
      final Socket s = serverSocket.accept();
      conn.bind(s, params);
      threadPool.submit(new SentiloHttpRequestTask(httpService, conn));
    } catch (final SocketException se) {
      // LOGGER.warn("An error has been raised while waiting to new requests on port {}", port);
    }

  }

  private void stopThreadPool() {
    try {
      if (threadPool != null) {
        threadPool.shutdown();
      }
    } catch (final Exception e) {
      LOGGER.warn("Error stopping thread pool.", e);
    }

    LOGGER.warn("Thread pool shutdown");
  }

  private void releaseSocketPort() {
    try {
      if (serverSocket != null && !serverSocket.isClosed()) {
        serverSocket.close();
      }
    } catch (final IOException e) {
      LOGGER.warn("Error closing socket on port {}", port, e);
    }

    LOGGER.info("Listener off on port {}", port);
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

  public ThreadPool getThreadPool() {
    return threadPool;
  }

  public void setThreadPool(final ThreadPool threadPool) {
    this.threadPool = threadPool;
  }

  public int getSocketTcpBacklog() {
    return socketTcpBacklog;
  }

  public void setSocketTcpBacklog(final int socketTcpBacklog) {
    this.socketTcpBacklog = socketTcpBacklog;
  }

}
