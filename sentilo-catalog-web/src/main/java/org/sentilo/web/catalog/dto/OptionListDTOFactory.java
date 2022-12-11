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
package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.domain.AlphabeticalSortable;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

public class OptionListDTOFactory {

  public static <T> List<OptionDTO> build(final List<T> items) {
    final List<OptionDTO> target = new ArrayList<OptionDTO>();

    for (final T item : items) {
      // item could be either a String or an AlphabeticalSortable, ....
      if (item instanceof String) {
        target.add(build((String) item));
      } else if (item instanceof AlphabeticalSortable) {
        target.add(build((AlphabeticalSortable) item));
      }
    }

    return target;
  }

  public static List<OptionDTO> build(final String[] suffixes, final String prefix, final MessageSource messageSource) {
    final List<OptionDTO> target = new ArrayList<OptionDTO>();
    for (final String value : suffixes) {
      final String trimValue = value.trim();
      final String label = messageSource.getMessage(prefix.concat(".").concat(trimValue), null, trimValue, LocaleContextHolder.getLocale());
      target.add(new OptionDTO(label, trimValue));
    }

    return target;
  }

  private static OptionDTO build(final String source) {
    return new OptionDTO(source, source);
  }

  private static OptionDTO build(final AlphabeticalSortable source) {
    final String value = source.getId();
    final String label = source.getSortableValue();

    return new OptionDTO(label, value);
  }

}
