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
package org.sentilo.agent.metrics.monitor.repository.batch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public class BatchProcessWorker implements Callable<BatchProcessResult> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BatchProcessWorker.class);
  private static final String DEFAULT_INDEX_NAME = "metrics-sentilo";
  private static final String DEFAULT_INDEX_MATH_DATE_PATTERN = "{now/d}";

  private List<String> initialMetricsToProcess;
  private final RESTClient restClient;
  private int numRetries;
  private final String indexName;
  private final int numMaxRetries;
  private final String esVersion;
  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  public BatchProcessWorker(final BatchProcessContext batchUpdateContext) {
    initialMetricsToProcess = batchUpdateContext.getMetricsToProcess();
    restClient = batchUpdateContext.getRestClient();
    numMaxRetries = batchUpdateContext.getNumMaxRetries();
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
    LOGGER.debug("Init batch process. Event elements that should be indexed in elasticsearch:  {} ", initialMetricsToProcess.size());

    final List<String> metricsNoProcessed = doBatchProcess();
    final int numElementsProcessed = initialMetricsToProcess.size() - metricsNoProcessed.size();
    LOGGER.debug("Number of elements actually indexed: {}", numElementsProcessed);

    final BatchProcessResult result = new BatchProcessResult(initialMetricsToProcess.size(), metricsNoProcessed);

    if (!CollectionUtils.isEmpty(result.getPendingMetrics())) {
      LOGGER.warn("Number of elements not indexed: {}", result.getNumElementsToProcess());
      LOGGER.warn("This metrics will not be indexed: {}", result.getPendingMetrics());
    }

    return result;
  }

  private List<String> doBatchProcess() {
    List<String> metricsToProcess = initialMetricsToProcess;

    while (!CollectionUtils.isEmpty(metricsToProcess) && checkRetries()) {
      final BatchProcessResponse response = callBulkApi(metricsToProcess);
      metricsToProcess = processResponse(response, metricsToProcess);
    }

    return metricsToProcess;
  }

  private BatchProcessResponse callBulkApi(final List<String> metricsToProcess) {
    numRetries++;
    LOGGER.debug("Num of attempt: {}", numRetries);
    BatchProcessResponse response = null;
    try {
      final RequestContext rc = new RequestContext("/_bulk", buildBody(metricsToProcess));
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
          + "Metrics no indexed will be stored for further processing.", numRetries, numMaxRetries);
      return false;
    }
  }

  /**
   * Returns the body associated with the bulk call
   *
   * @param eventsToProcess
   * @return
   */
  private String buildBody(final List<String> metricsToProcess) {
    final StringBuilder sb = new StringBuilder();

    // https://www.elastic.co/guide/en/elasticsearch/reference/6.0/breaking-changes-6.0.html
    // The ability to have multiple mapping types per index has been removed in 6.0
    // https://www.elastic.co/guide/en/elasticsearch/reference/7.0/removal-of-types.html
    // Since 7.0 mappings types are deprecated
    final boolean addTypeAttribute =
        StringUtils.hasText(esVersion) && (esVersion.startsWith("2.") || esVersion.startsWith("5.") || esVersion.startsWith("6."));
    final String mappingType = addTypeAttribute && esVersion.startsWith("2") ? "metrics" : "_doc";

    for (final String metrics : metricsToProcess) {
      if (addTypeAttribute) {
        sb.append("{ \"index\" : { \"_index\" : \"" + indexName + "\", \"_type\" : \"" + mappingType + "\" }}");
      } else {
        sb.append("{ \"index\" : { \"_index\" : \"" + indexName + "\" }}");
      }
      sb.append("\n");
      sb.append(metrics);
      sb.append("\n");
    }

    return sb.toString();
  }

  /**
   * Read response returned by EL and returns a new list with the documents (metrics) that have not
   * been indexed.
   *
   * @param response Response to the bulk index call.
   * @param metricsToProcess Documents sent to be indexed
   * @return
   */
  private List<String> processResponse(final BatchProcessResponse response, final List<String> metricsToProcess) {
    // The Elasticsearch response contains the items array, which lists the result of each
    // request, in the same order as we requested them:
    if (response == null || response.hasAllItemsRejected()) {
      return metricsToProcess;
    } else if (!response.hasErrors()) {
      return Collections.emptyList();
    } else {
      final List<String> aux = new ArrayList<>();
      for (int i = 0; i < response.getItems().size(); i++) {
        if (response.getItems().get(i).getStatus() != 201) {
          aux.add(metricsToProcess.get(i));
        }
      }
      return aux;
    }
  }

}
