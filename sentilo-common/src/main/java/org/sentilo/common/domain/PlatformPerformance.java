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
package org.sentilo.common.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

public class PlatformPerformance {

  @JsonInclude(value = Include.NON_NULL)
  private String tenant;
  private boolean isMaster;
  private float instantAvg;
  private float maxDailyAvg;
  private float maxAvg;
  private long timestamp;
  private long totalRequests;
  private long totalObs;
  private long totalOrders;
  private long totalAlarms;

  public PlatformPerformance() {
    super();
  }

  public PlatformPerformance(final PlatformActivity activity, final float instantAvg, final float maxDailyAvg, final float maxAvg,
      final long timestamp) {
    this();
    totalRequests = activity.getTotalRequests();
    totalObs = activity.getTotalObs();
    totalOrders = activity.getTotalOrders();
    totalAlarms = activity.getTotalAlarms();
    totalObs = activity.getTotalObs();
    isMaster = activity.isMaster();
    tenant = activity.getTenant();

    this.instantAvg = instantAvg;
    this.maxDailyAvg = maxDailyAvg;
    this.maxAvg = maxAvg;
    this.timestamp = timestamp;
  }

  public String getTenant() {
    return tenant;
  }

  public void setTenant(final String tenant) {
    this.tenant = tenant;
  }

  @JsonProperty("isMaster")
  public boolean isMaster() {
    return isMaster;
  }

  @JsonProperty("isMaster")
  public void setMaster(final boolean isMaster) {
    this.isMaster = isMaster;
  }

  public float getInstantAvg() {
    return instantAvg;
  }

  public void setInstantAvg(final float instantAvg) {
    this.instantAvg = instantAvg;
  }

  public float getMaxDailyAvg() {
    return maxDailyAvg;
  }

  public void setMaxDailyAvg(final float maxDailyAvg) {
    this.maxDailyAvg = maxDailyAvg;
  }

  public float getMaxAvg() {
    return maxAvg;
  }

  public void setMaxAvg(final float maxAvg) {
    this.maxAvg = maxAvg;
  }

  public long getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(final long timestamp) {
    this.timestamp = timestamp;
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

}
