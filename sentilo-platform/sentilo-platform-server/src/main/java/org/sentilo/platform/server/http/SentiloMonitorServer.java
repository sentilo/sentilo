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
import java.util.concurrent.TimeUnit;

import javax.annotation.PreDestroy;

import org.apache.http.ExceptionLogger;
import org.apache.http.config.SocketConfig;
import org.apache.http.impl.bootstrap.HttpServer;
import org.apache.http.impl.bootstrap.ServerBootstrap;
import org.sentilo.platform.server.request.SentiloRequestHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SentiloMonitorServer {

  private static final Logger LOGGER = LoggerFactory.getLogger(SentiloMonitorServer.class);

  private boolean listening;
  private int port;
  private HttpServer monitorServer;
  private SocketConfig socketConfig;

  private SentiloRequestHandler requestHandler;


  public void run() {
    try {
      initialize();

      // By default, this server has an executor configured with 1 core thread and 1 maximum thread
      monitorServer = ServerBootstrap.bootstrap().setListenerPort(getPort()).setServerInfo("Sentilo Monitor Server/1.1").setSocketConfig(socketConfig)
          .setExceptionLogger(ExceptionLogger.STD_ERR).registerHandler("*", requestHandler).create();

      LOGGER.info("Starting Monitor server on port {}", getPort());
      setListening(true);
      // Start server to accept incoming requests (internally it controls thread state. See
      // RequestListener.run )
      monitorServer.start();
      // Block SentiloMonitorServer thread and keep server listening for incoming requests until
      // shutdown is call
      monitorServer.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);

    } catch (final IOException ioe) {
      LOGGER.error("Error while initializing server thread. {}", ioe);
    } catch (final Exception e) {
      LOGGER.error("Error while running sentilo monitor server on port {}. {}", getPort(), e);
    } finally {
      setListening(false);
      cleanUp();
    }
  }

  @PreDestroy
  private void cleanUp() {
    monitorServer.shutdown(10, TimeUnit.SECONDS);
  }

  private void initialize() throws IOException {
    LOGGER.info("Initializing monitor server");

    createSocketConfig();
  }

  private void createSocketConfig() {
    socketConfig = SocketConfig.custom().setSoTimeout(15000).setTcpNoDelay(true).build();
  }

  public int getPort() {
    return port;
  }

  public void setPort(final int port) {
    this.port = port;
  }

  public boolean isListening() {
    return listening;
  }

  public void setListening(final boolean listening) {
    this.listening = listening;
  }

  public void setRequestHandler(final SentiloRequestHandler requestHandler) {
    this.requestHandler = requestHandler;
  }
}
