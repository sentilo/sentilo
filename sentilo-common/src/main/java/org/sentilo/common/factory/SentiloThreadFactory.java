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
package org.sentilo.common.factory;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

import io.netty.util.concurrent.DefaultThreadFactory;

/**
 * Default {@link ThreadFactory} implementation based on {@link DefaultThreadFactory} but allows
 * build threads following a given naming rule (useful for auditory purposes).
 */
public class SentiloThreadFactory implements ThreadFactory {

  private static final AtomicInteger poolNumber = new AtomicInteger();

  private final AtomicInteger threadNumber = new AtomicInteger();
  private final String namePrefix;
  private final boolean daemon = false;
  private final int priority = Thread.NORM_PRIORITY;
  protected final ThreadGroup threadGroup;

  public SentiloThreadFactory(final String groupName, final String threadPrefix) {
    super();
    threadGroup = new ThreadGroup(groupName);
    namePrefix = threadPrefix + "-" + poolNumber.incrementAndGet() + "-";
  }

  @Override
  public Thread newThread(final Runnable runnable) {
    final Thread t = new Thread(threadGroup, runnable, namePrefix + threadNumber.incrementAndGet(), 0);
    t.setDaemon(daemon);
    t.setPriority(priority);
    return t;
  }
}
