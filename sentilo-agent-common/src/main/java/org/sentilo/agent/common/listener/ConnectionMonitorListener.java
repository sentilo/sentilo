/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.agent.common.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.SmartLifecycle;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.Topic;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import redis.clients.jedis.exceptions.JedisConnectionException;

/**
 * El objetivo de esta clase es comprobar que la conexion que utiliza el listenerContainer, para
 * mantenerse subscrito a los datos, esta Ok. Para ello lo que se hace es subscribirse al canal
 * MONITOR al arrancar la plataforma y enviar periodicamente un dato asi como comprobar el estado de
 * la conexión de subscripcion. Si en cualquier momento se detecta un problema en esta conexion, se
 * reinicia el listener.
 */
@Component
public class ConnectionMonitorListener implements SmartLifecycle {

  private final Logger logger = LoggerFactory.getLogger(ConnectionMonitorListener.class);
  private static final int INITIAL_DELAY = 120000;
  private static final int FIXED_DELAY = 30000;

  @Autowired
  private RedisMessageListenerContainer listenerContainer;

  // whether the monitor is running (or not)
  private volatile boolean running = false;

  // Monitor message listener and topic
  private AbstractMessageListenerImpl monitorListener = null;
  private Topic monitorTopic = null;

  public ConnectionMonitorListener() {
    super();
  }

  @Override
  public void start() {
    if (listenerContainer.isActive() && listenerContainer.isRunning()) {
      final String agentName = System.getProperty("sentilo.agent.name");
      monitorListener = new MonitorMessageListenerImpl(buildMonitorMessageListenerName(agentName));
      monitorTopic = new PatternTopic(buildMonitorChannelName(agentName));
      running = true;
      logger.debug("ConnectionMonitorListener started");
    }
  }

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void validateConnection() {
    if (!isRunning()) {
      start();
    }

    if (isRunning()) {
      try {
        // Es importante tener en cuenta que la conexion de subscripcion solo permite invocar a un
        // cjto reducido de comandos de Redis.
        // Por lo que simplemente nos subscribimos a un canal, y, si todo va bien, nos
        // desubscribimos.
        subscribeMonitor();
        removeMonitorSubscription();
        logger.info("Subscribe connection is OK!");
      } catch (final RedisConnectionFailureException e) {
        // Si se produce cualquier problema en el proceso de subscripcion/desubscripcion lo que
        // hacemos es lo siguiente:
        // 1. Parar/cancelar el thread de subscripcion que utiliza el container para que deseche la
        // conexión a Redis subyacente que internamente
        // utiliza ya que probablemente sea invalida y de aqui el error. Esto lo hacemos invocando
        // al metodo stop del listenerContainer
        // 2. Reiniciamos la conexión a Redis del container: simplemente se trata de volver a
        // invocar al metodo lazyListen. Y este metodo se
        // puede invocar mediante el registro de un listener cuando el listener no esta escuchando
        // (i.e., lo que hace la
        // llamada subscribeMonitor). También se puede hacer invocando al metodo start del
        // listenerContainer.
        logger.warn("Found error {} monitoring listener container. We proceed to restart the container", e.getMessage());
        restartListenerContainer();
      } catch (final JedisConnectionException e) {
        // Idem
        logger.warn("Found error {} monitoring listener container. We proceed to restart the container", e.getMessage());
        restartListenerContainer();
      } catch (final Exception e) {
        logger.warn("Error validating subscription connection", e);
      }
    }
  }

  private void restartListenerContainer() {
    logger.debug("Stopping listener container");
    try {
      listenerContainer.stop();
    } catch (final Exception re) {
      logger.warn("Found error {} stopping listener container. If it is not running, we proceed to start it", re.getMessage());
    }
    logger.debug("Starting listener container");
    if (!listenerContainer.isRunning()) {
      listenerContainer.start();
    }
  }

  private void subscribeMonitor() {
    logger.debug("Subscribing listener {} to channel {}", monitorListener.getName(), monitorTopic.getTopic());
    listenerContainer.addMessageListener(monitorListener, monitorTopic);
  }

  private void removeMonitorSubscription() {
    logger.debug("Removing listener {} from channel {}", monitorListener.getName(), monitorTopic.getTopic());
    listenerContainer.removeMessageListener(monitorListener, monitorTopic);
  }

  @Override
  public void stop() {
    // Remove subscription to channel MONITOR
    if (isRunning()) {
      try {
        listenerContainer.removeMessageListener(monitorListener, monitorTopic);
        logger.debug("{} -- Subscription to {} removed", monitorListener.getName(), monitorTopic.getTopic());
      } catch (final Exception e) {
      }
      running = false;
    }

    logger.debug("ListenerConnectionMonitor stopped");
  }

  private String buildMonitorChannelName(final String agentName) {
    return "/MONITOR/" + agentName.toUpperCase() + "_AGENT*";
  }

  private String buildMonitorMessageListenerName(final String agentName) {
    return agentName.toUpperCase() + "_AGENT_MONITOR";
  }

  @Override
  public boolean isRunning() {
    return running;
  }

  @Override
  public int getPhase() {
    // start the latest
    return Integer.MAX_VALUE;
  }

  @Override
  public boolean isAutoStartup() {
    return true;
  }

  @Override
  public void stop(final Runnable callback) {
    stop();
    callback.run();
  }
}
