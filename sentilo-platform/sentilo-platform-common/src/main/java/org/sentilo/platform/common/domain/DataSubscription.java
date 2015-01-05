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
package org.sentilo.platform.common.domain;

import org.sentilo.common.domain.SubscribeType;
import org.springframework.util.StringUtils;

public class DataSubscription extends Subscription {

  private final String providerId;
  private String sensorId;

  public DataSubscription(final String entityId, final String endpoint, final String providerId) {
    super(entityId, providerId, endpoint, SubscribeType.DATA);
    this.providerId = providerId;
  }

  public DataSubscription(final String entityId, final String endpoint, final String providerId, final String sensorId) {
    this(entityId, endpoint, providerId);
    this.sensorId = sensorId;
  }

  @Override
  public boolean hasResourceIdentified() {
    return (StringUtils.hasText(providerId) || StringUtils.hasText(sensorId));
  }

  public String getProviderId() {
    return providerId;
  }

  public String getSensorId() {
    return sensorId;
  }

}
