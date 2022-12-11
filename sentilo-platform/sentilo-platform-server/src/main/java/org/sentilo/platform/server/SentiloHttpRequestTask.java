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
package org.sentilo.platform.server;

import java.io.IOException;

import org.apache.http.HttpServerConnection;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SentiloHttpRequestTask implements Runnable {

  private static final Logger LOGGER = LoggerFactory.getLogger(SentiloHttpRequestTask.class);

  private final HttpServerConnection conn;
  private final HttpService httpService;
  private final HttpContext context = new BasicHttpContext(null);

  public SentiloHttpRequestTask(final HttpService service, final HttpServerConnection conn) throws IOException {
    httpService = service;
    this.conn = conn;
  }

  @Override
  public void run() {
    try {
      httpService.handleRequest(conn, context);
    } catch (final Exception e) {
      LOGGER.error("Error while handling request: {}", e.getMessage(), e);
    } finally {
      // This disable persistent connections from clients because always close them.
      // If persistent connections (i.e. connections with keep-alive header) are no closed
      // some clients get a Connection Timeout error while read response.
      closeConnection();
    }
  }

  private void closeConnection() {
    try {
      if (conn != null) {
        // Close connection gracefully to evict Connection reset errors in client side
        conn.close();
      }
    } catch (final IOException unmanaged) {
    }
  }
}
