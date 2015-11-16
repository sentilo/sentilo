/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.activity.monitor.repository.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import org.sentilo.agent.activity.monitor.parser.BatchProcessResponseConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.parser.EventMessageConverter;
import org.sentilo.common.rest.RESTClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BatchProcessWorker implements Callable<BatchProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessWorker.class);

  private List<EventMessage> initialEventsToProcess;
  private final RESTClient restClient;
  private final BatchProcessCallback callback;
  private int numRetries;
  private final int numMaxRetries;
  private final EventMessageConverter converter = new EventMessageConverter();
  private final BatchProcessResponseConverter responseConverter = new BatchProcessResponseConverter();

  public BatchProcessWorker(final BatchProcessContext batchUpdateContext) {
    initialEventsToProcess = batchUpdateContext.getEventsToProcess();
    restClient = batchUpdateContext.getRestClient();
    numMaxRetries = batchUpdateContext.getNumMaxRetries();
    callback = batchUpdateContext.getCallback();
  }

  public BatchProcessResult call() {
    LOGGER.info("Init batch process. Event elements that should be indexed in elasticsearch:  {} ", initialEventsToProcess.size());

    final List<EventMessage> eventsNoProcessed = doBatchProcess();
    final int numElementsProcessed = initialEventsToProcess.size() - eventsNoProcessed.size();
    LOGGER.info("Number of elements actually indexed: {}", numElementsProcessed);

    final BatchProcessResult result = new BatchProcessResult(initialEventsToProcess.size(), eventsNoProcessed);

    callback.notifyBatchProcessIsDone(result);
    return result;
  }

  private List<EventMessage> doBatchProcess() {
    List<EventMessage> eventsToProcess = initialEventsToProcess;

    while (checkRetries()) {
      final BatchProcessResponse response = callBulkApi(eventsToProcess);
      if (response.hasErrors()) {
        eventsToProcess = processResponse(response, eventsToProcess);
      } else {
        break;
      }
    }

    return eventsToProcess;
  }

  private BatchProcessResponse callBulkApi(final List<EventMessage> eventsToProcess) {
    numRetries++;
    LOGGER.debug("Num of attempt: {}", numRetries);
    BatchProcessResponse response = null;
    try {
      response = responseConverter.unmarshall(restClient.post("/sentilo/_bulk", buildBody(eventsToProcess)));
    } catch (final Exception e) {
      LOGGER.warn("Error executing batch process: {}", e.getMessage(), e);
      response = new BatchProcessResponse(true);
    }

    return response;
  }

  private boolean checkRetries() {
    if (numRetries < numMaxRetries) {
      return true;
    } else {
      LOGGER.error("Number of retries {} is greater or equals than the maximum number of retries configured {}. "
          + "Events no indexed will be stored for further processing.", numRetries, numMaxRetries);
      // Pendiente guardar en Redis

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
    final StringBuffer sb = new StringBuffer();

    for (final EventMessage event : eventsToProcess) {
      sb.append("{ \"index\" : { \"_index\" : \"sentilo\", \"_type\" : \"" + event.getType().toLowerCase() + "\" }}");
      sb.append("\n");
      sb.append(converter.marshall(event));
      sb.append("\n");
    }

    return sb.toString();
  }

  /**
   * Read response returned by EL and returns a new list with the documents (events) that have not
   * been indexed.
   * 
   * @param response Response to the bulk index call.
   * @param eventsToProcess Documents sent to be indexed
   * @return
   */
  private List<EventMessage> processResponse(final BatchProcessResponse response, final List<EventMessage> eventsToProcess) {
    // The Elasticsearch response contains the items array, which lists the result of each
    // request, in the same order as we requested them:
    if (response == null || response.hasAllItemsRejected()) {
      return eventsToProcess;
    } else if (!response.hasErrors()) {
      return Collections.emptyList();
    } else {
      final List<EventMessage> aux = new ArrayList<EventMessage>();
      for (int i = 0; i < response.getItems().size(); i++) {
        if (response.getItems().get(i).getStatus() != 201) {
          aux.add(eventsToProcess.get(i));
        }
      }
      return aux;
    }
  }
}
