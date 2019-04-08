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
package org.sentilo.platform.client.core.utils;

import java.util.List;

import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.rest.RequestContext;
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
import org.sentilo.platform.client.core.domain.PlatformClientInputMessage;
import org.sentilo.platform.client.core.domain.SubscribeInputMessage;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.ObjectArrays;

public abstract class RequestUtils {

  private RequestUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static RequestContext buildContext(final PlatformClientInputMessage message) {
    final String path = buildPath(message);
    final RequestContext rc = new RequestContext(path);
    rc.setIdentityToken(message.getIdentityToken());
    return rc;
  }

  public static RequestContext buildContext(final PlatformClientInputMessage message, final String body) {
    final RequestContext rc = buildContext(message);
    rc.setBody(body);
    return rc;
  }

  public static RequestContext buildContext(final PlatformClientInputMessage message, final RequestParameters parameters) {
    final RequestContext rc = buildContext(message);
    rc.setParameters(parameters);
    return rc;
  }

  public static RequestParameters buildParameters(final PlatformSearchInputMessage message) {
    RequestParameters parameters = null;

    if (message.hasQueryFilters()) {
      parameters =
          RequestParameters.build(message.getQueryFilters().getFrom(), message.getQueryFilters().getTo(), message.getQueryFilters().getLimit());
    }

    return parameters;
  }

  private static String buildPath(final PlatformClientInputMessage message) {
    final String[] apiServiceTokens = getApiService(message);
    final String[] additionalPathTokens = getAdditionalPathTokens(message);

    return buildPath(message.getResourcesValues(),
        additionalPathTokens == null ? apiServiceTokens : ObjectArrays.concat(apiServiceTokens, additionalPathTokens, String.class));
  }

  private static String[] getAdditionalPathTokens(final PlatformClientInputMessage message) {
    if (message instanceof SubscribeInputMessage && ((SubscribeInputMessage) message).getType() != null) {
      return new String[] {((SubscribeInputMessage) message).getType().name().toLowerCase()};
    } else {
      return null;
    }
  }

  private static String[] getApiService(final PlatformClientInputMessage message) {
    if (message instanceof DataInputMessage) {
      return new String[] {SentiloConstants.DATA_TOKEN};
    } else if (message instanceof OrderInputMessage) {
      return new String[] {SentiloConstants.ORDER_TOKEN};
    } else if (message instanceof AlarmInputMessage) {
      return new String[] {SentiloConstants.ALARM_TOKEN};
    } else if (message instanceof SubscribeInputMessage) {
      return new String[] {SentiloConstants.SUBSCRIBE_TOKEN};
    } else if (message instanceof CatalogAlertInputMessage) {
      return new String[] {SentiloConstants.CATALOG_TOKEN, SentiloConstants.ALERT_TOKEN};
    } else if (message instanceof CatalogInputMessage) {
      return new String[] {SentiloConstants.CATALOG_TOKEN};
    } else if (message instanceof CatalogDeleteInputMessage) {
      return new String[] {SentiloConstants.CATALOG_TOKEN};
    } else {
      final String templateMessage = "Unknown PlatformInputMessage class %s";
      throw new IllegalArgumentException(String.format(templateMessage, message.getClass()));
    }
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
