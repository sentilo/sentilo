/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.utils;

import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * A tuple of 3 things.
 *
 * @see org.springframework.data.util.Pair
 */
public final class Tuple3<R, S, T> {

  private final R first;
  private final S second;
  private final T third;

  private Tuple3(final R first, final S second, final T third) {

    Assert.notNull(first, "First must not be null!");
    Assert.notNull(second, "Second must not be null!");
    Assert.notNull(third, "Third must not be null!");

    this.first = first;
    this.second = second;
    this.third = third;
  }

  /**
   * Creates a new {@link Pair} for the given elements.
   *
   * @param first must not be {@literal null}.
   * @param second must not be {@literal null}.
   * @return
   */
  public static <R, S, T> Tuple3<R, S, T> of(final R first, final S second, final T third) {
    return new Tuple3<>(first, second, third);
  }

  /**
   * Returns the first element of the {@link Tuple3}.
   *
   * @return
   */
  public R getFirst() {
    return first;
  }

  /**
   * Returns the second element of the {@link Tuple3}.
   *
   * @return
   */
  public S getSecond() {
    return second;
  }

  /**
   * Returns the third element of the {@link Tuple3}.
   *
   * @return
   */
  public T getThird() {
    return third;
  }

  /*
   * (non-Javadoc)
   *
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(@Nullable final Object o) {

    if (this == o) {
      return true;
    }

    if (!(o instanceof Tuple3)) {
      return false;
    }

    final Tuple3<?, ?, ?> tuple = (Tuple3<?, ?, ?>) o;

    if (!ObjectUtils.nullSafeEquals(first, tuple.first)) {
      return false;
    } else if (!ObjectUtils.nullSafeEquals(second, tuple.second)) {
      return false;
    } else {
      return ObjectUtils.nullSafeEquals(third, tuple.third);
    }

  }

  /*
   * (non-Javadoc)
   *
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int result = ObjectUtils.nullSafeHashCode(first);
    result = 31 * result + 7 * ObjectUtils.nullSafeHashCode(second) + 37 * ObjectUtils.nullSafeHashCode(third);
    return result;
  }

  /*
   * (non-Javadoc)
   *
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return String.format("%s->%s->%s", this.first, this.second, this.third);
  }
}
