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
package org.sentilo.agent.alert.listener;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.sentilo.agent.alert.service.AlertService;
import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.factory.SentiloThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

/**
 * Get input events and dispatch them between all clients subscribed to its topic
 */
@Component
public class MessageListenerImpl extends AbstractMessageListenerImpl {

  private static final Logger LOGGER = LoggerFactory.getLogger(MessageListenerImpl.class);

  private static final int DEFAULT_NUM_MAX_WORKERS = 5;

  @Autowired
  private AlertService alertService;

  @Nullable
  private ExecutorService taskExecutor;

  @Value("${sentilo.agent.batch.size:10}")
  private int batchSize;

  @Value("${sentilo.agent.batch.workers.size.min:0}")
  private int numMinWorkers;

  @Value("${sentilo.agent.batch.workers.size.max:5}")
  private int numMaxWorkers;

  public MessageListenerImpl(final String name) {
    super(name);
  }

  public MessageListenerImpl() {
    this("Alert listener");
  }

  @PostConstruct
  public void init() {
    if (numMaxWorkers == 0) {
      numMaxWorkers = DEFAULT_NUM_MAX_WORKERS;
    }

    if (numMinWorkers > numMaxWorkers) {
      LOGGER.info("Field numMinWorkers is greater that numMaxWorkers. Setting numMinWorkers to {}", numMaxWorkers);
      numMinWorkers = numMaxWorkers;
    }

    if (taskExecutor == null) {
      taskExecutor = new ThreadPoolExecutor(numMinWorkers, numMaxWorkers, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(),
          new SentiloThreadFactory("Agent Alert", "EventDispatcherWorker"));
    }

  }

  @Override
  public void doWithMessage(final EventMessage eventMessage) {
    // When alertService is active, dispatches input events between all clients subscribed to event
    // topic
    if (alertService.isActive()) {
      final SensorAlertsRuleEngine sare = alertService.getRuleEngine(eventMessage.getTopic());
      if (sare != null) {
        taskExecutor.execute(() -> processMessage(sare, eventMessage));
      }
    } else {
      final String errorMessage = "Agent still is not ready to process events. It is loading ....";
      LOGGER.info(errorMessage);
      throw new IllegalStateException(errorMessage);
    }
  }

  /**
   * Process a message received from the provider.
   *
   * @param message
   */
  protected void processMessage(final SensorAlertsRuleEngine sare, final EventMessage eventMessage) {
    try {
      sare.process(eventMessage);
    } catch (final Throwable ex) {
      LOGGER.debug("Rule engine exception after dispatching message to {}", sare.getName());
    }
  }

}
