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

import java.util.ArrayList;
import java.util.List;

public class DataTablesDTO {

  private Integer sEcho;
  private Long iTotalRecords;
  private Long iTotalDisplayRecords;
  private Long totalCount;
  private List<List<String>> aaData;

  public DataTablesDTO() {
    aaData = new ArrayList<List<String>>();
  }

  public Integer getsEcho() {
    return sEcho;
  }

  public void setsEcho(final Integer sEcho) {
    this.sEcho = sEcho;
  }

  public Long getiTotalRecords() {
    return iTotalRecords;
  }

  public void setiTotalRecords(final Long iTotalRecords) {
    this.iTotalRecords = iTotalRecords;
  }

  public Long getiTotalDisplayRecords() {
    return iTotalDisplayRecords;
  }

  public void setiTotalDisplayRecords(final Long iTotalDisplayRecords) {
    this.iTotalDisplayRecords = iTotalDisplayRecords;
  }

  public void add(final List<String> row) {
    aaData.add(row);
  }

  public List<List<String>> getAaData() {
    return aaData;
  }

  public void setAaData(final List<List<String>> aaData) {
    this.aaData = aaData;
  }

  public Long getTotalCount() {
    return totalCount;
  }

  public void setTotalCount(final Long totalCount) {
    this.totalCount = totalCount;
  }
}
