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
package org.sentilo.platform.service.impl;

import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogAlertResponseMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.exception.RESTClientException;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.common.domain.EntitiesMetadataMessage;
import org.sentilo.platform.common.domain.PermissionsMessage;
import org.sentilo.platform.common.exception.CatalogAccessException;
import org.sentilo.platform.common.service.CatalogService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.NestedRuntimeException;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class CatalogServiceImpl implements CatalogService {

  private static final Logger LOGGER = LoggerFactory.getLogger(CatalogServiceImpl.class);

  @Autowired
  @Qualifier("restClientImpl")
  private RESTClient restClient;

  private final StringMessageConverter converter = new DefaultStringMessageConverter();

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#getPermissions()
   */
  public PermissionsMessage getPermissions() {
    try {
      final RequestContext rc = new RequestContext(buildApiPath(SentiloConstants.PERMISSIONS_TOKEN));
      final String response = restClient.get(rc);
      return (PermissionsMessage) converter.unmarshal(response, PermissionsMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#getEntitiesMetadata()
   */
  public EntitiesMetadataMessage getEntitiesMetadata() {
    try {
      final RequestContext rc = new RequestContext(buildApiPath(SentiloConstants.METADATA_TOKEN));
      final String response = restClient.get(rc);
      return (EntitiesMetadataMessage) converter.unmarshal(response, EntitiesMetadataMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }

  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.CatalogService#insertSensors(org.sentilo.common.domain.
   * CatalogInputMessage)
   */
  public CatalogResponseMessage insertSensors(final CatalogInputMessage message) {
    try {
      final String path = buildApiPath(SentiloConstants.PROVIDER_TOKEN, message.getProviderId());
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.post(rc);
      return (CatalogResponseMessage) converter.unmarshal(response, CatalogResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.CatalogService#updateSensorsOrComponents(org.sentilo.common
   * .domain.CatalogInputMessage)
   */
  public CatalogResponseMessage updateSensorsOrComponents(final CatalogInputMessage message) {
    try {
      final String path = buildApiPath(SentiloConstants.PROVIDER_TOKEN, message.getProviderId());
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.put(rc);
      return (CatalogResponseMessage) converter.unmarshal(response, CatalogResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.CatalogService#getAuthorizedProviders(org.sentilo.common
   * .domain.CatalogInputMessage)
   */
  public CatalogResponseMessage getAuthorizedProviders(final CatalogInputMessage message) {
    try {
      final RequestParameters parameters = new RequestParameters();
      final String path = buildApiPath(SentiloConstants.AUTHORIZED_TOKEN, SentiloConstants.PROVIDER_TOKEN, message.getProviderId());
      final RequestContext rc = new RequestContext(path);

      if (!CollectionUtils.isEmpty(message.getParameters())) {
        parameters.put(message.getParameters());
        rc.setParameters(parameters);
      }

      final String response = restClient.get(rc);
      return (CatalogResponseMessage) converter.unmarshal(response, CatalogResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.common.service.CatalogService#deleteProvider(org.sentilo.common.domain
   * .CatalogInputMessage)
   */
  public CatalogResponseMessage deleteProvider(final CatalogInputMessage message) {
    try {
      final String path = buildApiPath(SentiloConstants.DELETE_TOKEN, SentiloConstants.PROVIDER_TOKEN, message.getProviderId());
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.put(rc);
      return (CatalogResponseMessage) converter.unmarshal(response, CatalogResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#getAlertsOwners()
   */
  public CatalogAlertResponseMessage getAlertsOwners() {
    try {
      final RequestContext rc = new RequestContext(buildApiPath(SentiloConstants.ALERT_TOKEN, SentiloConstants.OWNERS_TOKEN));
      final String response = restClient.get(rc);
      return (CatalogAlertResponseMessage) converter.unmarshal(response, CatalogAlertResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#getAuthorizedAlerts(org.sentilo.common.
   * domain .CatalogAlertInputMessage)
   */
  public CatalogAlertResponseMessage getAuthorizedAlerts(final CatalogAlertInputMessage message) {
    try {
      final RequestParameters parameters = new RequestParameters();
      final String path = buildApiPath(SentiloConstants.ALERT_TOKEN, SentiloConstants.ENTITY_TOKEN, message.getEntityId());
      final RequestContext rc = new RequestContext(path);

      if (!CollectionUtils.isEmpty(message.getParameters())) {
        parameters.put(message.getParameters());
        rc.setParameters(parameters);
      }

      final String response = restClient.get(rc);
      return (CatalogAlertResponseMessage) converter.unmarshal(response, CatalogAlertResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#insertAlerts(org.sentilo.common.domain.
   * CatalogAlertInputMessage)
   */
  public CatalogAlertResponseMessage insertAlerts(final CatalogAlertInputMessage message) {
    try {
      final String path = buildApiPath(SentiloConstants.ALERT_TOKEN, SentiloConstants.ENTITY_TOKEN, message.getEntityId());
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.post(rc);
      return (CatalogAlertResponseMessage) converter.unmarshal(response, CatalogAlertResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#updateAlerts(org.sentilo.common.domain.
   * CatalogAlertInputMessage)
   */
  public CatalogAlertResponseMessage updateAlerts(final CatalogAlertInputMessage message) {
    try {
      final String path = buildApiPath(SentiloConstants.ALERT_TOKEN, SentiloConstants.ENTITY_TOKEN, message.getEntityId());
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.put(rc);
      return (CatalogAlertResponseMessage) converter.unmarshal(response, CatalogAlertResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.common.service.CatalogService#deleteAlerts(org.sentilo.common.domain.
   * CatalogAlertInputMessage)
   */
  public CatalogAlertResponseMessage deleteAlerts(final CatalogAlertInputMessage message) {
    try {
      final String path =
          buildApiPath(SentiloConstants.ALERT_TOKEN, SentiloConstants.ENTITY_TOKEN, message.getEntityId(), SentiloConstants.DELETE_TOKEN);
      final RequestContext rc = new RequestContext(path, message.getBody());
      final String response = restClient.put(rc);
      return (CatalogAlertResponseMessage) converter.unmarshal(response, CatalogAlertResponseMessage.class);
    } catch (final NestedRuntimeException rce) {
      throw translateException(rce);
    }
  }

  private String buildApiPath(final String... pathTokens) {
    final StringBuilder sb = new StringBuilder(SentiloConstants.API_TOKEN);

    for (final String pathToken : pathTokens) {
      sb.append(SentiloConstants.SLASH).append(pathToken);
    }

    return sb.toString();
  }

  private CatalogAccessException translateException(final NestedRuntimeException nre) {
    LOGGER.warn("Translating exception of type {} ", nre.getClass());
    final String credentialsErrorMessage =
        "Bad credentials invoking Catalog: you should review the catalog.rest.credentials value at your integration.properties config file";
    final String errorMessage =
        nre instanceof RESTClientException && ((RESTClientException) nre).getStatus() == 401 ? credentialsErrorMessage : nre.getMessage();
    return new CatalogAccessException(errorMessage, nre);
  }

}
