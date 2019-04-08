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
package org.sentilo.agent.common.test.server;

import java.lang.reflect.Field;
import java.util.IdentityHashMap;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.agent.common.server.SentiloAgentServer;
import org.sentilo.agent.common.utils.Constants;
import org.sentilo.common.hook.SentiloShutdownHook;

public class SentiloAgentServerTest {

  @Test
  public void main() throws Exception {
    final String[] args = {"mockContextSpring.xml"};
    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, "mockAgent");
    System.setProperty("spring.profiles.active", "mockProfile");

    SentiloAgentServer.main(args);

    Assert.assertTrue(sentiloHookDownIsRegistered());
  }

  @Test(expected = IllegalArgumentException.class)
  public void mainWithInvalidArguments() {
    final String[] args = {"mockContextSpring.xml"};
    System.setProperty(Constants.SENTILO_AGENT_NAME_ENV, "Mock.Agent");
    SentiloAgentServer.main(args);
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  private boolean sentiloHookDownIsRegistered() throws Exception {
    // Hooks repository is a class without public access, so we must use reflection to retrieve
    // the list of hooks registered
    boolean sentiloHookFound = false;
    final Class clazz = Class.forName("java.lang.ApplicationShutdownHooks");
    final Field field = clazz.getDeclaredField("hooks");
    field.setAccessible(true);
    final IdentityHashMap<Thread, Thread> hooks = (IdentityHashMap<Thread, Thread>) field.get(null);

    for (final Thread hook : hooks.keySet()) {
      if (hook instanceof SentiloShutdownHook) {
        sentiloHookFound = true;
        break;
      }
    }

    return sentiloHookFound;
  }

}
