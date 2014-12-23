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
package org.sentilo.platform.service.listener;

import org.sentilo.common.utils.SentiloConstants;
import org.springframework.util.StringUtils;

public class NotificationParams {

  private final String endpoint;
  private final String secretCallbackKey;

  public NotificationParams(final String notificationParamsChain) {
    // notificationParamsChain has the format endpoint#@#secret where
    // secret could be optional
    final String[] params = notificationParamsChain.split(SentiloConstants.SENTILO_INTERNAL_TOKEN);
    endpoint = params[0];
    secretCallbackKey = (params.length == 2 ? params[1] : null);
  }

  public NotificationParams(final String endpoint, final String secretCallbackKey) {
    this.endpoint = endpoint;
    this.secretCallbackKey = secretCallbackKey;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("endpoint:" + endpoint);
    if (StringUtils.hasText(secretCallbackKey)) {
      sb.append("secretCallbackKey: *****");
    }
    return sb.toString();
  }

  public String getEndpoint() {
    return endpoint;
  }

  public String getSecretCallbackKey() {
    return secretCallbackKey;
  }
}
