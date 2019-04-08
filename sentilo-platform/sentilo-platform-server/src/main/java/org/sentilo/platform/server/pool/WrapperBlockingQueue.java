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
package org.sentilo.platform.server.pool;

import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class WrapperBlockingQueue implements BlockingQueue<Runnable> {

  private final LinkedBlockingQueue<Runnable> blockingQueue;

  public WrapperBlockingQueue(final int capacity) {
    blockingQueue = new LinkedBlockingQueue<Runnable>(capacity);
  }

  public boolean add(final Runnable e) {
    return blockingQueue.add(e);
  }

  public boolean addAll(final Collection<? extends Runnable> c) {
    return blockingQueue.addAll(c);
  }

  public void clear() {
    blockingQueue.clear();
  }

  public boolean contains(final Object o) {
    return blockingQueue.contains(o);
  }

  public boolean containsAll(final Collection<?> c) {
    return blockingQueue.containsAll(c);
  }

  public int drainTo(final Collection<? super Runnable> c, final int maxElements) {
    return blockingQueue.drainTo(c, maxElements);
  }

  public int drainTo(final Collection<? super Runnable> c) {
    return blockingQueue.drainTo(c);
  }

  public Runnable element() {
    return blockingQueue.element();
  }

  public boolean equals(final Object obj) {
    return blockingQueue.equals(obj);
  }

  public int hashCode() {
    return blockingQueue.hashCode();
  }

  public boolean isEmpty() {
    return blockingQueue.isEmpty();
  }

  public Iterator<Runnable> iterator() {
    return blockingQueue.iterator();
  }

  public boolean offer(final Runnable e, final long timeout, final TimeUnit unit) throws InterruptedException {
    return blockingQueue.offer(e, timeout, unit);
  }

  public boolean offer(final Runnable e) {
    return false;
  }

  public boolean backdoorOffer(final Runnable e) {
    return blockingQueue.offer(e);
  }

  public Runnable peek() {
    return blockingQueue.peek();
  }

  public Runnable poll() {
    return blockingQueue.poll();
  }

  public Runnable poll(final long timeout, final TimeUnit unit) throws InterruptedException {
    return blockingQueue.poll(timeout, unit);
  }

  public void put(final Runnable e) throws InterruptedException {
    blockingQueue.put(e);
  }

  public int remainingCapacity() {
    return blockingQueue.remainingCapacity();
  }

  public Runnable remove() {
    return blockingQueue.remove();
  }

  public boolean remove(final Object o) {
    return blockingQueue.remove(o);
  }

  public boolean removeAll(final Collection<?> c) {
    return blockingQueue.removeAll(c);
  }

  public boolean retainAll(final Collection<?> c) {
    return blockingQueue.retainAll(c);
  }

  public int size() {
    return blockingQueue.size();
  }

  public Runnable take() throws InterruptedException {
    return blockingQueue.take();
  }

  public Object[] toArray() {
    return blockingQueue.toArray();
  }

  public <T> T[] toArray(final T[] a) {
    return blockingQueue.toArray(a);
  }

  public String toString() {
    return blockingQueue.toString();
  }

}
