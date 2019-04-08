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
package org.sentilo.web.catalog.utils;

import org.springframework.util.Assert;

public abstract class CompoundKeyBuilder {

  private CompoundKeyBuilder() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String buildCompoundKey(final String... tokens) {
    Assert.notEmpty(tokens, "Key tokens must not be null or empty");
    Assert.noNullElements(tokens, "Key tokens must not be null");
    boolean firstToken = true;
    final StringBuilder sb = new StringBuilder();
    for (final String token : tokens) {
      if (!firstToken) {
        sb.append(Constants.DEFAULT_KEY_TOKEN_SPLITTER);
      }
      sb.append(token);

      firstToken = false;
    }

    return sb.toString();
  }

  public static String[] splitCompoundKey(final String compoundKey) {
    Assert.notNull(compoundKey, "Compound key must not be null");
    return compoundKey.split("\\" + Constants.DEFAULT_KEY_TOKEN_SPLITTER);

  }

}
