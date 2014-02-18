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
package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Provider;

public class CredentialsDTO {

  private List<CredentialDTO> credentials;

  public CredentialsDTO() {
    credentials = new ArrayList<CredentialDTO>();
  }

  public CredentialsDTO(final List<CredentialDTO> credential) {
    credentials = credential;
  }

  public List<CredentialDTO> getCredentials() {
    return credentials;
  }

  public void setCredentials(final List<CredentialDTO> credentials) {
    this.credentials = credentials;
  }

  public void addAll(final List<CredentialDTO> credentials) {
    this.credentials.addAll(credentials);
  }

  public void addAllApplications(final List<Application> applications) {
    for (final Application app : applications) {
      credentials.add(new CredentialDTO(app.getId(), app.getToken()));
    }
  }

  public void addAllProviders(final List<Provider> providers) {
    for (final Provider provider : providers) {
      credentials.add(new CredentialDTO(provider.getId(), provider.getToken()));
    }
  }

  public void add(final CredentialDTO credential) {
    credentials.add(credential);
  }

}
