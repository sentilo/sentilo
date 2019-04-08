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

import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

public final class LastUpdateMessageBuilder {

  public static String buildMessage(final MessageSource messageSource, final long lastUpdatedTime) {
    // If lastUpdatedTime == 0, it means that the component never has been updated or that the last
    // updated time is greater than an upper bound. Anyway, it means that the last update time is
    // unknown.
    final String messageKeyBase = "component.lastupdated.time.";
    final String[] messageKeySuffixes = {"unknown", "days", "hours", "minutes", "seconds"};

    final int secondMilliseconds = 1000;
    final int minutMilliseconds = secondMilliseconds * 60;
    final int hourMilliseconds = minutMilliseconds * 60;
    final int dayMilliseconds = hourMilliseconds * 24;
    final int[] timeReferences = {dayMilliseconds, hourMilliseconds, minutMilliseconds, secondMilliseconds};

    // Message to return must be referenced to the current time
    final long lastUpdatedTimeScaled = lastUpdatedTime == 0 ? lastUpdatedTime : System.currentTimeMillis() - lastUpdatedTime;

    final int pos = getBaseTimeReference(lastUpdatedTimeScaled, timeReferences);

    final String messageKey = messageKeyBase.concat(messageKeySuffixes[pos + 1]);
    final long timeUnits = pos != -1 ? lastUpdatedTimeScaled / timeReferences[pos] : 0;

    return messageSource.getMessage(messageKey, new Object[] {timeUnits}, LocaleContextHolder.getLocale());

  }

  /**
   * Returns the time reference (seconds, minutes, hours or days) that represents the time defined
   * by <code>lastUpdatedTime</code> parameter . <code>timeReferences</code> defines the list of
   * time references sorted in descendent order (days, hours, ...)
   *
   * @param lastUpdatedTime
   * @param timeReferences
   * @return
   */
  private static int getBaseTimeReference(final long lastUpdatedTime, final int[] timeReferences) {
    int pos = -1;

    for (int i = 0; i < timeReferences.length; i++) {
      if (lastUpdatedTime / timeReferences[i] != 0) {
        pos = i;
        break;
      }
    }

    return pos;
  }
}
