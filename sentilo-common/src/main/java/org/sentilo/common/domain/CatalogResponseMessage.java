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

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class CatalogResponseMessage {

  public static final String OK = "200";
  public static final String INTERNAL_SERVER_ERROR = "500";
  public static final String BAD_REQUEST = "400";
  public static final String FORBIDDEN = "403";

  @JsonInclude(value = Include.NON_EMPTY)
  private List<AuthorizedProvider> providers;

  @JsonInclude(value = Include.NON_EMPTY)
  private String code;
  @JsonInclude(value = Include.NON_EMPTY)
  private String errorMessage;

  @JsonInclude(value = Include.NON_EMPTY)
  private List<String> errorDetails;

  public CatalogResponseMessage() {
    super();
    code = OK;
  }

  public CatalogResponseMessage(final String errorMessage) {
    this();
    code = INTERNAL_SERVER_ERROR;
    this.errorMessage = errorMessage;
  }

  public CatalogResponseMessage(final String errorCode, final String errorMessage) {
    this(errorMessage);
    code = errorCode;
  }

  public CatalogResponseMessage(final String errorCode, final String errorMessage, final List<String> errorDetails) {
    this(errorMessage);
    code = errorCode;
    this.errorDetails = errorDetails;
  }

  public CatalogResponseMessage(final List<AuthorizedProvider> providers) {
    this();
    this.providers = providers;
  }

  public String getCode() {
    return code;
  }

  public void setCode(final String code) {
    this.code = code;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(final String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public void setProviders(final List<AuthorizedProvider> providers) {
    this.providers = providers;
  }

  public List<AuthorizedProvider> getProviders() {
    return providers;
  }

  public void setErrorDetails(final List<String> errorDetails) {
    this.errorDetails = errorDetails;
  }

  public List<String> getErrorDetails() {
    return errorDetails;
  }
}
