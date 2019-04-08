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
package org.sentilo.web.catalog.domain;

public class PlatformStatsMessage {

  private PlatformEvents events;
  private PlatformPerformance performance;

  public PlatformStatsMessage() {
    super();
  }

  public PlatformEvents getEvents() {
    return events;
  }

  public PlatformPerformance getPerformance() {
    return performance;
  }

  public static class PlatformEvents {

    private Long total;
    private Long observations;
    private Long alarms;
    private Long orders;

    public PlatformEvents() {
      super();
    }

    public Long getTotal() {
      return total;
    }

    public Long getObservations() {
      return observations;
    }

    public Long getAlarms() {
      return alarms;
    }

    public Long getOrders() {
      return orders;
    }

    public void setTotal(final Long total) {
      this.total = total;
    }

    public void setObservations(final Long observations) {
      this.observations = observations;
    }

    public void setAlarms(final Long alarms) {
      this.alarms = alarms;
    }

    public void setOrders(final Long orders) {
      this.orders = orders;
    }
  }

  public static class PlatformPerformance {

    private Float instantAvg;
    private Float dailyAvg;
    private Float maxAvg;

    public PlatformPerformance() {
      super();
    }

    public Float getInstantAvg() {
      return instantAvg;
    }

    public Float getDailyAvg() {
      return dailyAvg;
    }

    public Float getMaxAvg() {
      return maxAvg;
    }

    public void setInstantAvg(final Float instantAvg) {
      this.instantAvg = instantAvg;
    }

    public void setDailyAvg(final Float dailyAvg) {
      this.dailyAvg = dailyAvg;
    }

    public void setMaxAvg(final Float maxAvg) {
      this.maxAvg = maxAvg;
    }
  }

}
