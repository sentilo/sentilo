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
package org.sentilo.common.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.javacrumbs.shedlock.support.Utils;

public abstract class ModuleUtils {

  protected static final Logger LOGGER = LoggerFactory.getLogger(ModuleUtils.class);

  private ModuleUtils() {
    throw new AssertionError();
  }

  /**
   * All no-web Sentilo modules are either agent modules or API-Server module, and first ones all
   * have system property sentilo.agent.name filled in.
   *
   * @return Module name
   */
  public static String getModuleName() {
    return System.getProperty(SentiloConstants.SENTILO_AGENT_NAME_ENV, "API-Server").toLowerCase();
  }

  /**
   * Returns an unique identifier constructed with both module name and hostname of the machine
   * where the module is deployed. For example:
   * <code>sentilo:AWS-1234:agent.relational:config</code>
   *
   * @return unique module identifier
   */
  public static String buildUniqueModuleKey(final String moduleName, final String suffix) {
    // To differentiate instances of the same module, each hash key will be compounded by the
    // artifact name plus hostname
    final String hostname = Utils.getHostname();
    return String.format("sentilo:%s:%s:%s", hostname, moduleName, suffix);
  }

}
