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
package org.sentilo.agent.common.hook;

import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.SmartLifecycle;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;

public abstract class AgentLifecycleHook implements SmartLifecycle {

  private static final Logger LOGGER = LoggerFactory.getLogger(AgentLifecycleHook.class);

  private boolean running = false;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  @Override
  public void start() {
    LOGGER.info("AgentLifecycleHook started");
    running = true;
  }

  @Override
  public boolean isRunning() {
    return running;
  }

  @Override
  public int getPhase() {
    // Must be the first bean to shutdown
    return Integer.MAX_VALUE;
  }

  @Override
  public boolean isAutoStartup() {
    return true;
  }

  @Override
  public void stop() {
    LOGGER.info("Call stop method. Is hook running? {}", running);
    if (running) {
      try {
        // Stop the RedisMessageListenerContainer ...
        listenerContainer.stop();

        // ... wait 2 seconds so that all threads related to the listenerContainer get enough time
        // to do their cleanup ...
        TimeUnit.SECONDS.sleep(2);

        // ... and finally call custom agent stop process to ensure that no pending works are lost
        // during shutdown
        doGracefulShutdown();
      } catch (final Exception e) {
        LOGGER.error("Failed doing a graceful shutdown : {}", e.getMessage(), e);
      } finally {
        running = false;
        LOGGER.info("Hook stopped.");
      }
    }
  }

  @Override
  public void stop(final Runnable callback) {
    stop();
    callback.run();
  }

  protected abstract void doGracefulShutdown();

}
