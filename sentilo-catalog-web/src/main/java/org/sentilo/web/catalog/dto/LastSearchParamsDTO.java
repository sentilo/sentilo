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

import java.io.Serializable;

import org.sentilo.web.catalog.utils.Constants;

/**
 * DTO to store the parameters used by the SearchControler to execute the queries.
 */
public class LastSearchParamsDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String listName;
  private int pageNumber;
  private int pageSize;
  private String wordToSearch;
  private int sortColum = 1;
  private String sortDirection;

  public LastSearchParamsDTO() {
    super();
  }

  public LastSearchParamsDTO(final int pageNumber, final int pageSize, final String wordToSearch, final String listName, final int position,
      final String sortDirec) {
    this();
    this.pageNumber = pageNumber;
    this.pageSize = pageSize;
    this.wordToSearch = wordToSearch;
    this.listName = listName;
    sortColum = position;
    sortDirection = sortDirec;
  }

  /**
   * Initialize all paramaters except the parameter pageNumber which must be preserved between
   * requests.
   */
  public void clean() {
    pageNumber = 0;
    sortColum = 1;
    wordToSearch = null;
    sortDirection = Constants.ASC;
  }

  public int getPageNumber() {
    return pageNumber;
  }

  public void setPageNumber(final int pageNumber) {
    this.pageNumber = pageNumber;
  }

  public int getPageSize() {
    return pageSize;
  }

  public void setPageSize(final int pageSize) {
    this.pageSize = pageSize;
  }

  public String getWordToSearch() {
    return wordToSearch;
  }

  public void setWordToSearch(final String wordToSearch) {
    this.wordToSearch = wordToSearch;
  }

  public String getListName() {
    return listName;
  }

  public void setListName(final String listName) {
    this.listName = listName;
  }

  public int getSortColum() {
    return sortColum;
  }

  public void setSortColum(final int sortColum) {
    this.sortColum = sortColum;
  }

  public String getSortDirecction() {
    return sortDirection;
  }

  public void setSortDirecction(final String sortDirecction) {
    sortDirection = sortDirecction;
  }

}
