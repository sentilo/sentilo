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

import java.text.Collator;
import java.util.Comparator;

import org.sentilo.web.catalog.domain.AlphabeticalSortable;
import org.springframework.context.i18n.LocaleContextHolder;

public class AlphabeticalComparator implements Comparator<AlphabeticalSortable> {

  private final Collator collator;

  public AlphabeticalComparator() {
    collator = Collator.getInstance(LocaleContextHolder.getLocale());
  }

  @Override
  public int compare(final AlphabeticalSortable o1, final AlphabeticalSortable o2) {
    final String sv1 = o1.getSortableValue();
    final String sv2 = o2.getSortableValue();

    if (sv1 != null && sv2 != null) {
      return collator.compare(sv1, sv2);
    } else {
      return compareNullableValues(sv1, sv2);
    }
  }

  private int compareNullableValues(final String sv1, final String sv2) {
    if (sv1 != null && sv2 == null) {
      return 1;
    } else if (sv1 == null && sv2 != null) {
      return -1;
    } else {
      return 0;
    }
  }

}
