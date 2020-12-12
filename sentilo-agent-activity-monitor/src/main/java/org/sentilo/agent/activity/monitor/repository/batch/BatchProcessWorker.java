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
package org.sentilo.agent.activity.monitor.repository.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.EventMessage;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public class BatchProcessWorker implements Callable<BatchProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessWorker.class);
  private static final String DEFAULT_INDEX_NAME = "sentilo";
  private static final String DEFAULT_INDEX_MATH_DATE_PATTERN = "{now/d}";

  private List<EventMessage> initialEventsToProcess;
  private final RESTClient restClient;
  private final BatchProcessCallback callback;
  private int numRetries;
  private final String indexName;
  private final int numMaxRetries;
  private final String esVersion;
  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  public BatchProcessWorker(final BatchProcessContext batchUpdateContext) {
    initialEventsToProcess = batchUpdateContext.getEventsToProcess();
    restClient = batchUpdateContext.getRestClient();
    numMaxRetries = batchUpdateContext.getNumMaxRetries();
    callback = batchUpdateContext.getCallback();
    esVersion = batchUpdateContext.getEsVersion();

    final String suffixIndexName =
        System.getProperty("elasticsearch.index.name") == null ? DEFAULT_INDEX_NAME : System.getProperty("elasticsearch.index.name");
    final String indexMathDatePattern = System.getProperty("elasticsearch.index.date.pattern") == null ? DEFAULT_INDEX_MATH_DATE_PATTERN
        : System.getProperty("elasticsearch.index.date.pattern");

    // indexName will be dynamic and depends on the current day. Its final name is derived from a
    // dath-math-expression
    indexName = "<" + suffixIndexName + "-" + indexMathDatePattern + ">";
  }

  @Override
  public BatchProcessResult call() {
    LOGGER.debug("Init batch process. Event elements that should be indexed in elasticsearch:  {} ", initialEventsToProcess.size());

    final List<EventMessage> eventsNoProcessed = doBatchProcess();
    final int numElementsProcessed = initialEventsToProcess.size() - eventsNoProcessed.size();
    LOGGER.debug("Number of elements actually indexed: {}", numElementsProcessed);

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
    LOGGER.debug("Num of attempt: {}. Num of events to be indexed: {}", numRetries, eventsToProcess.size());
    BatchProcessResponse response = null;
    try {
      final RequestContext rc = new RequestContext("/_bulk", buildBody(eventsToProcess));
      final String jsonResponse = restClient.post(rc);
      response = (BatchProcessResponse) converter.unmarshal(jsonResponse, BatchProcessResponse.class);
    } catch (final Exception e) {
      LOGGER.warn("Error executing bulk request to elasticsearch: {}", e.getMessage(), e);
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
    final StringBuilder sb = new StringBuilder();

    // If elasticsearch version is <= 2.x, mapping type is determined by the event type (data, alarm
    // or order), but mapping type is always _doc
    // https://www.elastic.co/guide/en/elasticsearch/reference/6.0/breaking-changes-6.0.html
    // The ability to have multiple mapping types per index has been removed in 6.0
    // https://www.elastic.co/guide/en/elasticsearch/reference/7.0/removal-of-types.html
    // Since 7.0 mappings types are deprecated
    final boolean addTypeAttribute =
        StringUtils.hasText(esVersion) && (esVersion.startsWith("2.") || esVersion.startsWith("5.") || esVersion.startsWith("6."));
    final boolean customEventMapping = addTypeAttribute && esVersion.startsWith("2.");

    for (final EventMessage event : eventsToProcess) {
      if (addTypeAttribute) {
        final String mappingType = customEventMapping ? event.getType().toLowerCase() : "_doc";
        sb.append("{ \"index\" : { \"_index\" : \"" + indexName + "\", \"_type\" : \"" + mappingType + "\" }}");
      } else {
        sb.append("{ \"index\" : { \"_index\" : \"" + indexName + "\" }}");
      }
      sb.append("\n");
      sb.append(converter.marshal(event));
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
