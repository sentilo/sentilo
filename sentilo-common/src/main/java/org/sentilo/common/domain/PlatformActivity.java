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
package org.sentilo.common.domain;

import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;

public class PlatformActivity {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private String tenant;
  private long totalRequests;
  private long totalObs;
  private long totalOrders;
  private long totalAlarms;
  private long timestamp;
  private boolean isMaster;

  public PlatformActivity() {
    super();
  }

  public PlatformActivity(final String tenant, final long totalRequests, final long totalObs, final long totalOrders, final long totalAlarms,
      final long timestamp, final boolean isMaster) {
    this();
    this.tenant = tenant;
    this.totalRequests = totalRequests;
    this.totalObs = totalObs;
    this.totalOrders = totalOrders;
    this.totalAlarms = totalAlarms;
    this.timestamp = timestamp;
    this.isMaster = isMaster;
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  public long getTotalRequests() {
    return totalRequests;
  }

  public void setTotalRequests(final long totalRequests) {
    this.totalRequests = totalRequests;
  }

  public long getTotalObs() {
    return totalObs;
  }

  public void setTotalObs(final long totalObs) {
    this.totalObs = totalObs;
  }

  public long getTotalOrders() {
    return totalOrders;
  }

  public void setTotalOrders(final long totalOrders) {
    this.totalOrders = totalOrders;
  }

  public long getTotalAlarms() {
    return totalAlarms;
  }

  public void setTotalAlarms(final long totalAlarms) {
    this.totalAlarms = totalAlarms;
  }

  public long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final long timestamp) {
    this.timestamp = timestamp;
  }

  @JsonProperty("isMaster")
  public boolean isMaster() {
    return isMaster;
  }

  @JsonProperty("isMaster")
  public void setMaster(final boolean isMaster) {
    this.isMaster = isMaster;
  }

}
