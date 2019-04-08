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
package org.sentilo.web.catalog.dto;

/**
 * Define the visual customized params for a Tenant or User, such like date format, time zone or the
 * number of observations on the activity charts by default
 */
public class VisualConfigurationDTO {

  private String timeZone;

  private String dateFormatPattern;

  private Integer chartVisibleObservationsNumber;

  public VisualConfigurationDTO() {
    super();
  }

  public VisualConfigurationDTO(final String timeZone, final String dateFormatPattern, final Integer chartVisibleObservationsNumber) {
    super();
    this.timeZone = timeZone;
    this.dateFormatPattern = dateFormatPattern;
    this.chartVisibleObservationsNumber = chartVisibleObservationsNumber;
  }

  /**
   * @return the timeZone
   */
  public String getTimeZone() {
    return timeZone;
  }

  /**
   * @param timeZone the timeZone to set
   */
  public void setTimeZone(final String timeZone) {
    this.timeZone = timeZone;
  }

  /**
   * @return the dateFormatPattern
   */
  public String getDateFormatPattern() {
    return dateFormatPattern;
  }

  /**
   * @param dateFormatPattern the dateFormatPattern to set
   */
  public void setDateFormatPattern(final String dateFormatPattern) {
    this.dateFormatPattern = dateFormatPattern;
  }

  /**
   * @return the chartVisibleObservationsNumber
   */
  public Integer getChartVisibleObservationsNumber() {
    return chartVisibleObservationsNumber;
  }

  /**
   * @param chartVisibleObservationsNumber the chartVisibleObservationsNumber to set
   */
  public void setChartVisibleObservationsNumber(final Integer chartVisibleObservationsNumber) {
    this.chartVisibleObservationsNumber = chartVisibleObservationsNumber;
  }

}
