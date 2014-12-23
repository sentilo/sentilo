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

import java.util.List;

import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.RESTUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.OrderInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.springframework.util.CollectionUtils;

public abstract class RequestUtils {

  private RequestUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String buildPath(final OrderInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.ORDER_TOKEN);
  }

  public static String buildPath(final AlarmInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.ALARM_TOKEN);
  }

  public static String buildPath(final DataInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.DATA_TOKEN);
  }

  public static String buildPath(final SubscribeInputMessage message) {
    if (message != null && message.getType() != null) {
      return buildPath(message.getResourcesValues(), SentiloConstants.SUBSCRIBE_TOKEN, message.getType().name().toLowerCase());
    } else {
      return buildPath(message.getResourcesValues(), SentiloConstants.SUBSCRIBE_TOKEN);
    }
  }

  public static String buildPath(final CatalogInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.CATALOG_TOKEN);
  }

  public static String buildPath(final CatalogDeleteInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.CATALOG_TOKEN);
  }

  public static String buildPath(final CatalogAlertInputMessage message) {
    return buildPath(message.getResourcesValues(), SentiloConstants.CATALOG_TOKEN, SentiloConstants.ALERT_TOKEN);
  }

  public static RequestParameters buildParameters(final PlatformSearchInputMessage message) {
    RequestParameters parameters = null;

    if (message.hasQueryFilters()) {
      parameters =
          RequestParameters.build(message.getQueryFilters().getFrom(), message.getQueryFilters().getTo(), message.getQueryFilters().getLimit());
    }

    return parameters;
  }

  private static String buildPath(final List<String> resourceTokens, final String... rootTokens) {
    if (CollectionUtils.isEmpty(resourceTokens)) {
      return RESTUtils.buildPath(rootTokens);
    } else {
      final List<String> tokens = SentiloUtils.addValuesToBeginningList(resourceTokens, rootTokens);
      return RESTUtils.buildPath(tokens.toArray(new String[tokens.size()]));
    }
  }
}
