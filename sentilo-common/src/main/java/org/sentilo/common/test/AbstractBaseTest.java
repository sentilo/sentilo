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
package org.sentilo.common.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

import org.mockito.ArgumentMatcher;
import org.springframework.util.CollectionUtils;

public abstract class AbstractBaseTest {

  protected <T> List<T> generateRandomList(final Class<T> clazz) throws InstantiationException, IllegalAccessException {
    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(5);
    } while (size == 0);

    final List<T> randomList = new ArrayList<T>();
    for (int i = 0; i < size; i++) {
      randomList.add(clazz.newInstance());
    }

    return randomList;
  }

  protected <K, V> Map<K, V> generateRandomMap(final Class<K> keyClazz, final Class<V> valueClazz)
      throws InstantiationException, IllegalAccessException {
    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(5);
    } while (size == 0);

    final Map<K, V> randomMap = new HashMap<K, V>();
    for (int i = 0; i < size; i++) {
      randomMap.put(keyClazz.newInstance(), valueClazz.newInstance());
    }

    return randomMap;
  }

  protected class EqualListSizeQueryMatcher<E> extends ArgumentMatcher<List<E>> {

    private final Integer size;

    public EqualListSizeQueryMatcher(final Integer size) {
      this.size = size;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean matches(final Object obj) {
      final List<E> objects = (List<E>) obj;

      return objects.size() == size;
    }
  }

  protected class ListMatcher extends ArgumentMatcher<List<String>> {

    private final List<String> items;

    public ListMatcher(final List<String> items) {
      this.items = items;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean matches(final Object argument) {
      final List<String> values = (List<String>) argument;
      boolean matches = !CollectionUtils.isEmpty(values) && (values.size() == items.size());

      if (matches) {
        // Check if exists a value into values that it isn't in items
        matches = values.stream().filter(value -> !items.contains(value)).collect(Collectors.toList()).size() == 0;
      }

      return matches;
    }

  }

}
