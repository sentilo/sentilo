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
package org.sentilo.platform.service.utils;

import org.sentilo.common.domain.PlatformSearchInputMessage;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.util.StringUtils;

public abstract class QueryFilterParamsUtils {

  private QueryFilterParamsUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static Long getTo(final PlatformSearchInputMessage message) {
    if (!message.hasQueryFilters() || message.getQueryFilters().getTo() == null) {
      return System.currentTimeMillis();
    } else {
      return message.getQueryFilters().getTo().getTime();
    }
  }

  public static Long getFrom(final PlatformSearchInputMessage message) {
    if (!message.hasQueryFilters() || message.getQueryFilters().getFrom() == null) {
      return 0L;
    } else {
      return message.getQueryFilters().getFrom().getTime();
    }
  }

  public static Integer getLimit(final PlatformSearchInputMessage message) {
    // The maximum number of sensor elements (limit value) that could be returned in a search
    // depends on whether the search is by provider&sensor (limit <= NUM_MAX_ELEMENTS_SENSOR)
    // or only by provider (limit <= NUM_MAX_ELEMENTS_PROVIDER).

    // By default, it is fixed to DEFAULT_NUM_ELEMENTS

    if (!message.hasQueryFilters() || message.getQueryFilters().getLimit() == null) {
      return SentiloConstants.DEFAULT_NUM_ELEMENTS;
    } else if (StringUtils.hasText(message.getSensorId())) {
      return message.getQueryFilters().getLimit() > SentiloConstants.NUM_MAXIM_ELEMENTS_BY_SENSOR ? SentiloConstants.NUM_MAXIM_ELEMENTS_BY_SENSOR
          : message.getQueryFilters().getLimit();
    } else {
      return message.getQueryFilters().getLimit() > SentiloConstants.NUM_MAXIM_ELEMENTS_BY_PROVIDER ? SentiloConstants.NUM_MAXIM_ELEMENTS_BY_PROVIDER
          : message.getQueryFilters().getLimit();
    }
  }
}
