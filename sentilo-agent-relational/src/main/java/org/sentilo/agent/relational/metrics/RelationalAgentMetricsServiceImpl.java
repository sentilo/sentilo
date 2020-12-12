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
package org.sentilo.agent.relational.metrics;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.tomcat.jdbc.pool.DataSource;
import org.sentilo.agent.common.metrics.AgentMetricsServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
public class RelationalAgentMetricsServiceImpl extends AgentMetricsServiceImpl {

  public static final String DS_METRICS = "data_sources";
  public static final String DS_NAME = "name";
  public static final String DS_SIZE = "size";
  public static final String DS_MAX_ACTIVE = "max_active";
  public static final String DS_ACTIVE = "active";
  public static final String DS_MAX_IDLE = "max_idle";
  public static final String DS_MIN_IDLE = "min_idle";
  public static final String DS_IDLE = "idle";
  public static final String DS_WAIT_COUNT = "wait_count";

  @Autowired
  private Map<String, DataSource> dataSources = new HashMap<String, DataSource>();

  @Override
  protected Map<String, Object> collectCustomMetrics() {
    final Map<String, Object> agentMetrics = super.collectCustomMetrics();

    // Add relational custom metrics, e.g. connection pool state, ... ...
    if (!CollectionUtils.isEmpty(dataSources)) {
      final List<Map<String, Object>> ds = new ArrayList<Map<String, Object>>();
      for (final String key : dataSources.keySet()) {
        final DataSource dataSource = dataSources.get(key);
        final Map<String, Object> dsMap = new HashMap<String, Object>();
        dsMap.put(DS_NAME, key);
        dsMap.put(DS_SIZE, dataSource.getSize());
        dsMap.put(DS_MAX_ACTIVE, dataSource.getMaxActive());
        dsMap.put(DS_ACTIVE, dataSource.getActive());
        dsMap.put(DS_MAX_ACTIVE, dataSource.getMaxIdle());
        dsMap.put(DS_MIN_IDLE, dataSource.getMinIdle());
        dsMap.put(DS_IDLE, dataSource.getIdle());
        dsMap.put(DS_WAIT_COUNT, dataSource.getWaitCount());

        ds.add(dsMap);
      }

      agentMetrics.put(DS_METRICS, ds);
    }

    return agentMetrics;
  }
}
