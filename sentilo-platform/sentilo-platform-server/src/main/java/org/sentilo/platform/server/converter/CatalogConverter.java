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
package org.sentilo.platform.server.converter;

import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;

public class CatalogConverter extends PlatformJsonMessageConverter {

  public CatalogInputMessage parsePostRequest(final SentiloRequest request) {
    final String providerId = request.getResourcePart(0);
    final CatalogInputMessage inputMessage = readInternal(request, CatalogInputMessage.class);
    inputMessage.setProviderId(providerId);

    return inputMessage;
  }

  public CatalogInputMessage parsePutRequest(final SentiloRequest request) {
    final String providerId = request.getResourcePart(0);
    final CatalogInputMessage inputMessage = readInternal(request, CatalogInputMessage.class);
    inputMessage.setProviderId(providerId);

    return inputMessage;
  }

  public CatalogInputMessage parseGetRequest(final SentiloRequest request) {
    final String entityId = request.getEntitySource();
    final String providerId = request.getResourcePart(0) == null ? entityId : request.getResourcePart(0);

    final CatalogInputMessage inputMessage = new CatalogInputMessage(entityId, request.getParameters());
    inputMessage.setProviderId(providerId);

    return inputMessage;
  }

  public CatalogInputMessage parseDeleteRequest(final SentiloRequest request) {
    return parseDeleteRequest(request, false);
  }

  public CatalogInputMessage parseDeleteRequest(final SentiloRequest request, final boolean deleteSimulate) {
    final String providerId = request.getResourcePart(0);
    final CatalogDeleteInputMessage inputMessage =
        deleteSimulate ? (CatalogDeleteInputMessage) readInternal(request, CatalogDeleteInputMessage.class) : new CatalogDeleteInputMessage();
    inputMessage.setProviderId(providerId);

    return inputMessage;
  }

  protected CatalogInputMessage readInternal(final SentiloRequest request, final Class<? extends CatalogInputMessage> clazz) {
    try {
      // Input stream that contains request payload only could be read once time. Second time an
      // IOException will be thrown.
      final String body = request.getBody();
      final CatalogInputMessage message = (CatalogInputMessage) readInternal(clazz, body);
      message.setBody(body);
      return message;
    } catch (final Exception ex) {
      throw buildUnmarshallJsonException(clazz, ex);
    }
  }

  public void writeResponse(final SentiloResponse response, final CatalogResponseMessage message) {
    // To hide internal error code to client,we reset its value from the message
    message.setCode(null);
    writeInternal(message, response);
  }

}
