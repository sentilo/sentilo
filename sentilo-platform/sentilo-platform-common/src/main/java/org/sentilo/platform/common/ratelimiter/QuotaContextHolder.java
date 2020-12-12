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
package org.sentilo.platform.common.ratelimiter;

import java.util.HashMap;
import java.util.Map;

import org.springframework.core.NamedThreadLocal;
import org.springframework.util.CollectionUtils;

public class QuotaContextHolder {

  private static final ThreadLocal<QuotaContext> _contextHolder = new NamedThreadLocal<QuotaContext>("Quota Context");
  private static final ThreadLocal<Map<QuotaContext.Type, QuotaContext>> contextHolder =
      new NamedThreadLocal<Map<QuotaContext.Type, QuotaContext>>("Quota Context");

  /**
   * Obtain the current <code>QuotaContext</code>.
   * 
   * @return the quota context (could be <code>null</code>)
   */
  public static QuotaContext _getContext() {
    return _contextHolder.get();
  }

  public static void _setContext(final QuotaContext context) {
    _contextHolder.set(context);
  }

  public static boolean hasContext() {
    return !CollectionUtils.isEmpty(contextHolder.get());
  }

  public static boolean hasGlobalContext() {
    return hasContextType(QuotaContext.Type.GLOBAL);
  }

  public static boolean hasTenantContext() {
    return false;
  }

  public static boolean hasEntityContext() {
    return hasContextType(QuotaContext.Type.ENTITY);
  }

  private static boolean hasContextType(final QuotaContext.Type type) {
    return hasContext() && contextHolder.get().get(type) != null;
  }

  public static QuotaContext getContext(final QuotaContext.Type type) {
    return contextHolder.get().get(type);
  }

  /**
   * Associates a new <code>QuotaContext</code> with the current thread of execution.
   * 
   * @param context the new <code>QuotaContext</code> (may not be <code>null</code>)
   */

  public static void setContext(final QuotaContext context) {
    if (!hasContext()) {
      contextHolder.set(new HashMap<QuotaContext.Type, QuotaContext>());
    }

    contextHolder.get().put(context.getType(), context);
  }

  public static void clearContext() {
    contextHolder.remove();
  }
}
