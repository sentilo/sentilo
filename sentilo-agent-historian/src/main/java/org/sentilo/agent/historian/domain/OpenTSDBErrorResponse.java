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
package org.sentilo.agent.historian.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * Corresponds to the HTTP API error response when some of the datapoints could not be stored. See
 * <a href="http://opentsdb.net/docs/build/html/api_http/put.html"> http://opentsdb.net/docs/build/
 * html/api_http/put.html </a>
 */
public class OpenTSDBErrorResponse {

  private List<Error> errors = new ArrayList<Error>();
  private Integer failed;
  private Integer success;

  public List<Error> getErrors() {
    return errors;
  }

  public void setErrors(final List<Error> errors) {
    this.errors = errors;
  }

  public Integer getFailed() {
    return failed;
  }

  public void setFailed(final Integer failed) {
    this.failed = failed;
  }

  public Integer getSuccess() {
    return success;
  }

  public void setSuccess(final Integer success) {
    this.success = success;
  }

  public static class Error {

    public OpenTSDBDataPoint getDatapoint() {
      return datapoint;
    }

    public void setDatapoint(final OpenTSDBDataPoint datapoint) {
      this.datapoint = datapoint;
    }

    public String getError() {
      return error;
    }

    public void setError(final String error) {
      this.error = error;
    }

    private OpenTSDBDataPoint datapoint;
    private String error;
  }

}
