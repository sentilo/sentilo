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
package org.sentilo.web.catalog.context;

import org.sentilo.common.utils.SentiloConstants;
import org.springframework.core.NamedThreadLocal;
import org.springframework.util.StringUtils;

public class TenantContextHolder {

  private static final ThreadLocal<TenantContext> contextHolder = new NamedThreadLocal<TenantContext>("Tenant context");

  /**
   * Obtain the current <code>TenantContext</code>.
   *
   * @return the tenant context (could be <code>null</code>)
   */
  public static TenantContext getContext() {
    return contextHolder.get();
  }

  /**
   * Associates a new <code>TenantContext</code> with the current thread of execution.
   *
   * @param context the new <code>TenantContext</code> (may not be <code>null</code>)
   */
  public static void setContext(final TenantContext context) {
    contextHolder.set(context);
  }

  public static void clearContext() {
    contextHolder.remove();
  }

  public static boolean hasContext() {
    return isEnabled() && getContext() != null;
  }

  /**
   * Returns true if the environment variable sentilo.multitenant is true
   *
   * @return If this Sentilo instance is a multi-tenant instance.
   */
  public static boolean isEnabled() {
    final String propValue = System.getProperty(SentiloConstants.SENTILO_MULTITENANT_PROP_KEY);
    return StringUtils.hasText(propValue) && Boolean.valueOf(propValue);
  }

  /**
   * Returns true if #isEnabled() returns true and the environment variable
   * sentilo.multitenant.infer is true
   */
  public static boolean inferTenantFromLogin() {
    final String propValue = System.getProperty(SentiloConstants.SENTILO_MULTITENANT_INFER_PROP_KEY);
    return isEnabled() && StringUtils.hasText(propValue) && Boolean.valueOf(propValue);
  }

}
