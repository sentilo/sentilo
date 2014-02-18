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
package org.sentilo.platform.client.core.utils;

import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.PlatformClientInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;

public abstract class RequestUtils {

  private RequestUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String buildPath(final OrderInputMessage message) {
    final StringBuilder sb = new StringBuilder("/order");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final AlarmInputMessage message) {
    final StringBuilder sb = new StringBuilder("/alarm");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final DataInputMessage message) {
    final StringBuilder sb = new StringBuilder("/data");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final SubscribeInputMessage message) {
    final StringBuilder sb = new StringBuilder("/subscribe");

    if (message != null && message.getType() != null) {
      sb.append("/").append(message.getType().toString().toLowerCase());
    }

    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final CatalogInputMessage message) {
    final StringBuilder sb = new StringBuilder("/catalog");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final CatalogDeleteInputMessage message) {
    final StringBuilder sb = new StringBuilder("/catalog");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static String buildPath(final CatalogAlertInputMessage message) {
    final StringBuilder sb = new StringBuilder("/catalog/alert");
    sb.append(buildResourcePart(message));
    return sb.toString();
  }

  public static RequestParameters buildParameters(final PlatformSearchInputMessage message) {
    RequestParameters parameters = null;

    if (message.hasQueryFilters()) {
      parameters = RequestParameters.build(message.getQueryFilters().getFrom(), message.getQueryFilters().getTo(), message.getQueryFilters().getLimit());
    }

    return parameters;
  }

  private static String buildResourcePart(final PlatformClientInputMessage message) {
    final StringBuilder sb = new StringBuilder();

    for (final String param : message.getResourcesValues()) {
      sb.append("/").append(param);
    }

    return sb.toString();
  }
}
