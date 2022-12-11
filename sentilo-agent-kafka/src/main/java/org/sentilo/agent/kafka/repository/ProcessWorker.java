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
package org.sentilo.agent.kafka.repository;

import java.util.concurrent.Callable;

import org.sentilo.agent.kafka.utils.TopicName;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.util.concurrent.ListenableFuture;

public class ProcessWorker implements Callable<ProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProcessWorker.class);

  private final KafkaTemplate<String, String> kafkaTemplate;

  private final ProcessCallback callback;
  private int numRetries;
  private final int numMaxRetries;
  private final EventMessage eventToProcess;

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  public ProcessWorker(final ProcessContext streamUpdateContext) {
    eventToProcess = streamUpdateContext.getEventToProcess();
    numMaxRetries = streamUpdateContext.getNumMaxRetries();
    callback = streamUpdateContext.getCallback();
    kafkaTemplate = streamUpdateContext.getKafkaTemplate();
  }

  public ProcessResult call() {

    EventMessage failedEvent = null;

    if (eventToProcess != null && checkRetries()) {
      final boolean response = publishToKafka(eventToProcess);
      if (!response) {
        failedEvent = eventToProcess;
      }
    }

    final ProcessResult result = new ProcessResult(failedEvent);

    callback.notifyProcessIsDone(result);
    return result;
  }

  private boolean publishToKafka(final EventMessage eventToProcess) {
    numRetries++;
    LOGGER.debug("Num of attempt: {}", numRetries);

    boolean result = true;
    try {
      final String topicName = TopicName.createTopicName(eventToProcess);
      LOGGER.debug("Producing events to Kafka: {}, {}", topicName, buildBody(eventToProcess));

      final ListenableFuture<SendResult<String, String>> future = kafkaTemplate.send(topicName, buildBody(eventToProcess));
      future.get();

    } catch (final Exception e) {
      LOGGER.warn("Error executing stream process: {}", e.getMessage(), e);
      result = false;
    }

    return result;
  }

  private boolean checkRetries() {
    if (numRetries < numMaxRetries) {
      return true;
    } else {
      LOGGER.error("Number of retries {} is greater or equals than the maximum number of retries configured {}. "
          + "Events not exported will be stored for further processing.", numRetries, numMaxRetries);
      return false;
    }
  }

  /**
   * Returns the body associated with the bulk call
   *
   * @param eventsToProcess
   * @return
   */
  private String buildBody(final EventMessage eventToProcess) {

    return converter.marshal(eventToProcess);
  }

}
