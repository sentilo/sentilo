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
package org.sentilo.common.cache.impl;

import java.util.concurrent.TimeUnit;

import org.sentilo.common.cache.LRUCache;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

/**
 * Custom LRUCache, based initially on Google Cache library. In a future version could change
 * implementation but interface methods won't be changed (however new methods may be added at any
 * time).
 */
public class LRUCacheImpl<K, V> implements LRUCache<K, V> {

  private Cache<K, V> cache;

  public LRUCacheImpl(final int cacheSize) {
    this(cacheSize, 60);
  }

  public LRUCacheImpl(final int cacheSize, final int expireMinutes) {
    cache = CacheBuilder.newBuilder().maximumSize(cacheSize).expireAfterAccess(expireMinutes, TimeUnit.MINUTES).build();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#put(java.lang.Object, java.lang.Object)
   */
  public void put(final K key, final V item) {
    cache.put(key, item);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#get(java.lang.Object)
   */
  public V get(final K key) {
    return cache.getIfPresent(key);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#get(java.lang.Object, java.lang.Object)
   */
  public V get(final K key, final V defaultValue) {
    final V result = get(key);
    return result == null ? defaultValue : result;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#remove(java.lang.Object)
   */
  public void remove(final K key) {
    cache.invalidate(key);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#size()
   */
  public long size() {
    return cache.size();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.common.cache.LRUCache#contains(java.lang.Object)
   */
  public boolean contains(final K key) {
    return get(key) != null;
  }
}
