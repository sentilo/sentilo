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
package org.sentilo.platform.server.request;

import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

public abstract class RequestUtils {

  public static final String TOKEN_PATH_SEPARATOR = "/";
  public static final String EMPTY_STRING = "";
  public static final String[] EMPTY_ARRAY = {};

  private RequestUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String extractResource(final String path, final String handlerPath) {
    Assert.isTrue(path != null && path.startsWith(TOKEN_PATH_SEPARATOR), "Invalid path value. Must start with /");
    return handlerPath.length() == path.length() ? EMPTY_STRING : path.substring(handlerPath.length() + 1);
  }

  public static String[] splitPath(final String path) {
    Assert.isTrue(StringUtils.hasText(path) && path.startsWith(TOKEN_PATH_SEPARATOR), "Invalid path value. Must start with /");
    return path.split(TOKEN_PATH_SEPARATOR);
  }

  public static String[] splitResource(final String resource) {
    // Assert.isTrue(resource != null, "Invalid resource value. Must not be null");
    return StringUtils.hasText(resource) ? resource.split(TOKEN_PATH_SEPARATOR) : EMPTY_ARRAY;
  }

  public static String buildPath(final String... tokens) {
    final StringBuilder sb = new StringBuilder();
    for (final String token : tokens) {
      sb.append(TOKEN_PATH_SEPARATOR).append(token);
    }
    return sb.toString();
  }
}
