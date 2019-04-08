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
package org.sentilo.agent.relational.utils;

import org.sentilo.agent.relational.jdbc.SentiloRoutingDataSource;

/**
 * Thread local que nos proporciona un backend en el cual mantener el nombre del ds a utilizar en
 * cada uno de los datos a persistir. Este backend es utilizado en la clase
 * {@link SentiloRoutingDataSource} para poder hacer el lookup del ds correspondiente.
 *
 * @see SentiloRoutingDataSource
 */
public abstract class ThreadLocalProperties {

  public static final ThreadLocal<String> DS_THREAD_LOCAL = new ThreadLocal<String>();

  private ThreadLocalProperties() {
    throw new AssertionError();
  }

  public static void set(final String dsName) {
    DS_THREAD_LOCAL.set(dsName);
  }

  public static void unset() {
    DS_THREAD_LOCAL.remove();
  }

  public static String get() {
    return DS_THREAD_LOCAL.get();
  }

}
