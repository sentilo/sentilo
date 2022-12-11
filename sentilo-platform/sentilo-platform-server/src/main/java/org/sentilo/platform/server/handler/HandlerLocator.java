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
package org.sentilo.platform.server.handler;

import java.util.ArrayDeque;
import java.util.Hashtable;
import java.util.Map;
import java.util.Queue;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.server.request.RequestUtils;
import org.sentilo.platform.server.request.SentiloRequest;
import org.springframework.util.CollectionUtils;

public class HandlerLocator {

  private final Map<String, AbstractHandler> handlers;

  public HandlerLocator() {
    handlers = new Hashtable<String, AbstractHandler>();
  }

  public HandlerLocator(final Map<HandlerPath, AbstractHandler> mappingHandlers) {
    this();
    if (!CollectionUtils.isEmpty(mappingHandlers)) {
      mappingHandlers.forEach((k, v) -> register(k, v));
    }
  }

  public AbstractHandler lookup(final SentiloRequest request) {
    final String path = request.getPath();

    // If path is /token1/token2/ ... /tokenN , there are two possibilities to handlerPath value
    // /token1/token2
    // or
    // /token1
    AbstractHandler handler = null;
    final String[] tokens = RequestUtils.splitPath(path);
    if (!SentiloUtils.arrayIsEmpty(tokens)) {
      final Queue<String> handlerPathCandidates = new ArrayDeque<String>();
      if (tokens.length > 2) {
        handlerPathCandidates.add(RequestUtils.buildPath(tokens[1], tokens[2]));
      }
      handlerPathCandidates.add(RequestUtils.buildPath(tokens[1]));

      while (!CollectionUtils.isEmpty(handlerPathCandidates)) {
        final String handlerPath = handlerPathCandidates.poll();
        handler = handlers.get(handlerPath);

        if (handler != null) {
          final String resourcePath = RequestUtils.extractResource(path, handlerPath);
          request.setPathParts(handlerPath, resourcePath);
          break;
        }
      }
    }

    return handler;
  }

  public void register(final HandlerPath handler, final AbstractHandler handlerImpl) {
    add(handler.getPath(), handlerImpl);
  }

  public void add(final String path, final AbstractHandler handlerImpl) {
    handlers.put(path, handlerImpl);
  }

  public int size() {
    return handlers.size();
  }
}
