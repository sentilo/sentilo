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
package org.sentilo.agent.alert.service.impl;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;

import org.sentilo.agent.alert.domain.InternalAlert;
import org.sentilo.agent.alert.service.PublishService;
import org.sentilo.agent.alert.utils.Constants;
import org.sentilo.agent.common.metrics.AgentMetricsCounter;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.factory.SentiloThreadFactory;
import org.sentilo.platform.client.core.PlatformClientOperations;
import org.sentilo.platform.client.core.domain.AlarmInputMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * The responsibility of this class is to publish new alarms using API Rest's service /alarm. To do
 * this task more efficiently and improve its performance, uses a pool of workers to parallelize
 * publication and increment the number of alarms published.
 *
 */
@Component
public class PublishServiceImpl implements PublishService {

  private static final Logger LOGGER = LoggerFactory.getLogger(PublishServiceImpl.class);
  // Default number of workers: each worker is reponsable of publishes a new alarm via API Rest
  private static final int DEFAULT_NUM_MAX_WORKERS = 10;

  @Autowired
  private PlatformClientOperations platformClient;

  @Autowired
  private AgentMetricsCounter metricsCounters;

  private ExecutorService workersManager;

  @PostConstruct
  public void init() {
    if (workersManager == null) {
      workersManager = new ThreadPoolExecutor(0, DEFAULT_NUM_MAX_WORKERS, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>(),
          new SentiloThreadFactory("SentiloAlarmAgent", "PublishWorker"));
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.agent.alert.service.PublishService#publishFrozenAlarm(org.sentilo.agent.alert.
   * domain .InternalAlert)
   */
  @Override
  public void publishFrozenAlarm(final InternalAlert alert) {
    final String errorMessage = String.format(Constants.TEMPLATE_FROZEN_ALARM, alert.getId(), alert.getSensorId(), alert.getExpression());
    publishAlarm(alert, errorMessage);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.agent.alert.service.PublishService#publishAlarm(org.sentilo.agent.alert.domain.
   * InternalAlert, java.lang.String)
   */
  @Override
  public void publishAlarm(final InternalAlert alert, final String message) {
    LOGGER.debug("Publishing new alarm, related to (alert, sensor, provider) ({}, {} , {}), with message:  {}  ", alert.getId(), alert.getSensorId(),
        alert.getProviderId(), message);

    final AlarmInputMessage alarmMessage = new AlarmInputMessage(alert.getId(), message);
    alarmMessage.setProviderId(alert.getProviderId());
    alarmMessage.setSensorId(alert.getSensorId());
    alarmMessage.setAlertType(alert.getTrigger().name());

    final Runnable runnableTask = () ->
      {
        try {
          // Publish directly via API REST
          platformClient.getAlarmOps().publish(alarmMessage);
          metricsCounters.isRemoteServerConnectionOk(true);
          metricsCounters.incrementOutputEvents(1);
          LOGGER.info("Alarm {} has been successfully published.", alarmMessage.getMessage());
        } catch (final RESTClientException rce) {
          metricsCounters.isRemoteServerConnectionOk(false);
          LOGGER.error("An error has been detected while trying to publish alarm {} ", alarmMessage.getMessage(), rce);
        }
      };

    workersManager.execute(runnableTask);
  }

}
