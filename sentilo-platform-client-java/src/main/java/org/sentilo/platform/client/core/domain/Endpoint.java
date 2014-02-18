/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.platform.client.core.domain;

import org.codehaus.jackson.map.annotate.JsonSerialize;

public class Endpoint {

  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private final String endpoint;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private final String user;
  @JsonSerialize(include = JsonSerialize.Inclusion.NON_NULL)
  private final String password;

  public Endpoint(final String url) {
    this(url, null, null);
  }

  public Endpoint(final String url, final String user, final String password) {
    endpoint = url;
    this.user = user;
    this.password = password;
  }

  public String getEndpoint() {
    return endpoint;
  }

  public String getUser() {
    return user;
  }

  public String getPassword() {
    return password;
  }

}
