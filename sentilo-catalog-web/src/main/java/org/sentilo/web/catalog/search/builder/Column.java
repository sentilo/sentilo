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
package org.sentilo.web.catalog.search.builder;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Metadata about a column from a datatable.
 */
public class Column {

  private final String name;
  private boolean ordenable = false;
  private boolean searchable = false;
  private Map<String, Object> dictionary = new HashMap<String, Object>();

  public Column(final String name) {
    this(name, false);
  }

  public Column(final String name, final boolean ordenable) {
    this(name, ordenable, false);
  }

  public Column(final String name, final boolean ordenable, final boolean searchable) {
    this(name, ordenable, searchable, new HashMap<String, Object>());
  }

  public Column(final String name, final boolean ordenable, final boolean searchable, final Map<String, Object> dictionary) {
    super();
    this.name = name;
    this.ordenable = ordenable;
    this.searchable = searchable;
    this.dictionary = dictionary;
  }

  public String getName() {
    return name;
  }

  public boolean isOrdenable() {
    return ordenable;
  }

  public boolean isSearchable() {
    return searchable;
  }

  public Map<String, Object> getDictionary() {
    return dictionary;
  }

  public boolean isDictionaryEmpty() {
    return dictionary.isEmpty();
  }

  public Object dictionaryContainsWord(final String wordToSearch) {
    Object word = wordToSearch;
    final Iterator<Entry<String, Object>> it = dictionary.entrySet().iterator();
    while (it.hasNext()) {
      final Map.Entry<String, Object> dic = it.next();
      if (dic.getKey().contains(wordToSearch)) {
        word = dic.getValue();
        break;
      }
    }
    return word;
  }

}
