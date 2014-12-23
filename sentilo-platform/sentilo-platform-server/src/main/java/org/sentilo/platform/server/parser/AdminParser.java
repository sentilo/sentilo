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
package org.sentilo.platform.server.parser;

import java.util.List;

import org.sentilo.platform.common.domain.AdminInputMessage;
import org.sentilo.platform.common.domain.AdminInputMessage.AdminType;
import org.sentilo.platform.common.domain.Statistics;
import org.sentilo.platform.common.domain.Subscription;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;

public class AdminParser extends PlatformJsonMessageConverter {

  private final SubscribeParser subscribeParser = new SubscribeParser();

  public AdminInputMessage parseGetRequest(final SentiloRequest request) throws PlatformException {
    final AdminType type = getAdminType(request);
    final AdminInputMessage inputMessage = new AdminInputMessage(type);
    inputMessage.setEntity(request.getResourcePart(1));

    return inputMessage;
  }

  public AdminInputMessage parsePutRequest(final SentiloRequest request) throws PlatformException {
    final AdminType type = getAdminType(request);
    final AdminInputMessage inputMessage = (AdminInputMessage) readInternal(AdminInputMessage.class, request);
    inputMessage.setType(type);
    return inputMessage;
  }

  public void writeStatsResponse(final SentiloRequest request, final SentiloResponse response, final Statistics stats) throws PlatformException {
    writeInternal(stats, response);
  }

  public void writeSubscriptionsResponse(final SentiloRequest request, final SentiloResponse response, final List<Subscription> subscriptions)
      throws PlatformException {
    subscribeParser.writeResponse(response, subscriptions);
  }

  public AdminType getAdminType(final SentiloRequest request) {
    AdminType adminType = null;
    try {
      final String resourcePart = request.getResourcePart(0);
      if (StringUtils.hasText(resourcePart)) {
        adminType = AdminType.valueOf(resourcePart.toLowerCase());
      }
    } catch (final IllegalArgumentException e) {
    }

    return adminType;
  }

}
