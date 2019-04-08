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
package org.sentilo.agent.historian.repository.batch;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import org.apache.http.HttpStatus;
import org.sentilo.agent.historian.domain.OpenTSDBDataPoint;
import org.sentilo.agent.historian.domain.OpenTSDBErrorResponse;
import org.sentilo.agent.historian.utils.OpenTSDBDataPointMarshaller;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.exception.MessageConversionException;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

public class BatchProcessWorker implements Callable<BatchProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessWorker.class);

  private final List<EventMessage> initialEventsToProcess;
  private final RESTClient restClient;
  private final BatchProcessCallback callback;
  private int numRetries;
  private final int numMaxRetries;
  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  public BatchProcessWorker(final BatchProcessContext batchUpdateContext) {
    initialEventsToProcess = batchUpdateContext.getEventsToProcess();
    restClient = batchUpdateContext.getRestClient();
    numMaxRetries = batchUpdateContext.getNumMaxRetries();
    callback = batchUpdateContext.getCallback();

  }

  public BatchProcessResult call() {

    final List<EventMessage> eventsNoProcessed = doBatchProcess();
    final int numElementsProcessed = initialEventsToProcess.size() - eventsNoProcessed.size();
    LOGGER.debug("Number of elements stored in history: {}", numElementsProcessed);

    final BatchProcessResult result = new BatchProcessResult(initialEventsToProcess.size(), eventsNoProcessed);

    callback.notifyBatchProcessIsDone(result);
    return result;
  }

  private List<EventMessage> doBatchProcess() {
    List<EventMessage> eventsToProcess = initialEventsToProcess;

    while (!CollectionUtils.isEmpty(eventsToProcess) && checkRetries()) {
      final BatchProcessResponse response = callBulkApi(eventsToProcess);
      eventsToProcess = processResponse(response, eventsToProcess);
    }

    return eventsToProcess;
  }

  private BatchProcessResponse callBulkApi(final List<EventMessage> eventsToProcess) {
    numRetries++;
    LOGGER.debug("Num of attempt: {}", numRetries);
    BatchProcessResponse response = new BatchProcessResponse(false);
    try {
      final RequestContext rc = new RequestContext("/api/put?details", buildBody(eventsToProcess));
      restClient.post(rc);
    } catch (final RESTClientException e) {
      LOGGER.warn("Error executing batch process: {}", e.getMessage(), e);
      response = processRestClientException(response, e);
    } catch (final Exception e) {
      LOGGER.warn("Error executing batch process: {}", e.getMessage(), e);
      response = new BatchProcessResponse(true);
    }

    return response;
  }

  private BatchProcessResponse processRestClientException(BatchProcessResponse response, final RESTClientException e) {
    if (e.getStatus() == HttpStatus.SC_BAD_REQUEST) {
      try {
        // OpenTSDB returns a detailed message with list of failed metrics if the POST has
        // "details" param
        final OpenTSDBErrorResponse error = (OpenTSDBErrorResponse) converter.unmarshal(e.getMessage(), OpenTSDBErrorResponse.class);
        response.setErrorResponse(error);
      } catch (final MessageConversionException ee) {
        LOGGER.warn("Could not parse error response from OpenTSDB to expected format. Response was: {}", e.getMessage(), ee);
        response = new BatchProcessResponse(true);
      }
    } else {
      response = new BatchProcessResponse(true);
    }
    return response;
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
  private String buildBody(final List<EventMessage> eventsToProcess) {

    final List<OpenTSDBDataPoint> dataPoints = new ArrayList<OpenTSDBDataPoint>();

    for (final EventMessage event : eventsToProcess) {

      try {
        final OpenTSDBDataPoint dataPoint = OpenTSDBDataPointMarshaller.unmarshal(event);
        dataPoints.add(dataPoint);
      } catch (final ParseException pe) {
        LOGGER.error("Got error when trying to parse an event message: {}. EventMessage: {}", pe.getMessage(), event);
        continue;
      }

    }

    return converter.marshal(dataPoints);
  }

  /**
   * If some OpenTSDB datapoint entry fails OpenTSDB returns the whole DP representation. We can use
   * the returned DP to tell which EventMessage failed and try to resend it later.
   *
   * @param response Response to the bulk PUT.
   * @param initialEvents Event Messages originally sent to OpenTSDB
   * @return list of Event Messages that were not saved in OpenTSDB
   */
  private List<EventMessage> processResponse(final BatchProcessResponse response, final List<EventMessage> initialEvents) {
    if (response == null || response.hasAllItemsRejected()) {
      return initialEvents;
    } else if (response.getErrorResponse() == null) {
      return Collections.emptyList();
    } else {
      return filterProcessedEvents(response, initialEvents);
    }
  }

  private List<EventMessage> filterProcessedEvents(final BatchProcessResponse response, final List<EventMessage> initialEvents) {
    final List<EventMessage> eventsNoProcessed = new ArrayList<EventMessage>();
    for (final EventMessage event : initialEvents) {
      for (final OpenTSDBErrorResponse.Error error : response.getErrorResponse().getErrors()) {
        // compare by timestamp and measure name. Measure name in Sentilo is a unique string for
        // provider and sensor, so we can safely identify which data point belongs to which event
        // message here.
        if (event.getTime() == error.getDatapoint().getTimestamp() && event.getTopic().equals(error.getDatapoint().getMetric())) {
          eventsNoProcessed.add(event);
        }
      }
    }

    return eventsNoProcessed;
  }

}
