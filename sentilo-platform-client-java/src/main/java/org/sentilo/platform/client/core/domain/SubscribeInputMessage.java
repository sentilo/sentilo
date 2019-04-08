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
package org.sentilo.platform.client.core.domain;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.enums.SubscribeType;
import org.sentilo.platform.client.core.utils.ResourcesUtils;
import org.springframework.util.CollectionUtils;

public class SubscribeInputMessage implements PlatformClientInputMessage {

  public static final String ALARM_ID_KEY = "alarmId";
  public static final String PROVIDER_ID_KEY = "providerId";
  public static final String SENSOR_ID_KEY = "sensorId";

  /** Endpoint al cual se debera enviar las notificaciones mediante Http callback */
  private SubscriptionParams subscriptionParams;

  /** Tipo de subscripcion. */
  private final SubscribeType type;

  /**
   * Identificadores del recurso al cual esta asociado la subscripción: providerId, sensorId o
   * alarmId.
   */
  private final Map<String, String> resources = new HashMap<String, String>();

  /** Lista ordenada de los identificadores que forman el path del recurso. */
  private final List<String> resourcesValues = new ArrayList<String>();

  private String identityToken;

  public SubscribeInputMessage() {
    this(null);
  }

  public SubscribeInputMessage(final SubscribeType type) {
    this(null, type);
  }

  public SubscribeInputMessage(final SubscriptionParams subscriptionParams, final SubscribeType type) {
    super();
    this.subscriptionParams = subscriptionParams;
    this.type = type;
  }

  protected void addResource(final String key, final String value) {
    resources.put(key, value);
    ResourcesUtils.addToResources(value, getResourcesValues());
  }

  @Override
  public String getIdentityToken() {
    return identityToken;
  }

  @Override
  public void setIdentityToken(final String identityToken) {
    this.identityToken = identityToken;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append("--- Subscription ---");
    sb.append("\n\t type:").append(type);
    if (subscriptionParams != null) {
      sb.append("\n\t subscriptionParams:").append(subscriptionParams.toString());
    }
    if (!CollectionUtils.isEmpty(resources)) {
      sb.append("\n\t Resources:");
      sb.append("\n\t\t").append(resources.toString());
    }

    return sb.toString();
  }

  protected void setSubscriptionParams(final SubscriptionParams subscriptionParams) {
    this.subscriptionParams = subscriptionParams;
  }

  public SubscriptionParams getSubscriptionParams() {
    return subscriptionParams;
  }

  public SubscribeType getType() {
    return type;
  }

  public Map<String, String> getResources() {
    return resources;
  }

  @Override
  public List<String> getResourcesValues() {
    return resourcesValues;
  }
}
