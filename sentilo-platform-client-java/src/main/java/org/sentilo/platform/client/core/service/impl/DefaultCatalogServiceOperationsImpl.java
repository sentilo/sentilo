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
package org.sentilo.platform.client.core.service.impl;

import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.service.CatalogServiceOperations;
import org.sentilo.platform.client.core.utils.RequestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class DefaultCatalogServiceOperationsImpl extends AbstractServiceOperationsImpl implements CatalogServiceOperations {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultCatalogServiceOperationsImpl.class);

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.client.core.service.CatalogServiceOperations#getSensors(org.sentilo.
   * platform .client.core.domain.CatalogInputMessage)
   */
  @Override
  public CatalogOutputMessage getSensors(final CatalogInputMessage message) {
    LOGGER.debug("Retrieving authorized sensors from catalog ");
    final RequestParameters parameters = new RequestParameters();
    if (!CollectionUtils.isEmpty(message.getParameters())) {
      parameters.put(message.getParameters());
    }

    final RequestContext rc = RequestUtils.buildContext(message, parameters);
    final String response = getRestClient().get(rc);
    LOGGER.debug("Sensors retrieved ");

    return (CatalogOutputMessage) converter.unmarshal(response, CatalogOutputMessage.class);

  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#registerSensors(org.sentilo
   * .platform.client.core.domain.CatalogInputMessage)
   */
  @Override
  public void registerSensors(final CatalogInputMessage message) {
    LOGGER.debug("Registering sensors");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().post(rc);
    LOGGER.debug("Sensors registered ");
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#updateSensors(org.sentilo
   * .platform.client.core.domain.CatalogInputMessage)
   */
  @Override
  public void updateSensors(final CatalogInputMessage message) {
    LOGGER.debug("Updating sensors");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().put(rc);
    LOGGER.debug("Sensors updated");
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#updateComponents(org.sentilo
   * .platform.client.core.domain.CatalogInputMessage)
   */
  @Override
  public void updateComponents(final CatalogInputMessage message) {
    LOGGER.debug("Updating components");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().put(rc);
    LOGGER.debug("Components updated");

  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#deleteProvider(org.sentilo
   * .platform.client.core.domain.CatalogDeleteInputMessage)
   */
  @Override
  public void deleteProvider(final CatalogDeleteInputMessage message) {
    LOGGER.debug("Deleting provider components/sensors");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().delete(rc);
    LOGGER.debug("Provider components/sensors deleted");
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.platform.client.core.service.CatalogServiceOperations#getAuthorizedAlerts(org.
   * sentilo .platform.client.core.domain.CatalogAlertInputMessage)
   */
  @Override
  public CatalogAlertOutputMessage getAuthorizedAlerts(final CatalogAlertInputMessage message) {
    LOGGER.debug("Retrieving authorized alerts from catalog ");
    final RequestParameters parameters = new RequestParameters();
    if (!CollectionUtils.isEmpty(message.getParameters())) {
      parameters.put(message.getParameters());
    }

    final RequestContext rc = RequestUtils.buildContext(message, parameters);
    final String response = getRestClient().get(rc);
    LOGGER.debug("alerts retrieved ");

    return (CatalogAlertOutputMessage) converter.unmarshal(response, CatalogAlertOutputMessage.class);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#registerAlerts(org.sentilo
   * .platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void registerAlerts(final CatalogAlertInputMessage message) {
    LOGGER.debug("Registering alerts");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().post(rc);
    LOGGER.debug("alerts registered ");
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#updateAlerts(org.sentilo.
   * platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void updateAlerts(final CatalogAlertInputMessage message) {
    LOGGER.debug("Updating alerts");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().put(rc);
    LOGGER.debug("alerts updated ");

  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#deleteAlerts(org.sentilo.
   * platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void deleteAlerts(final CatalogAlertInputMessage message) {
    LOGGER.debug("Deleting alerts");
    final RequestContext rc = RequestUtils.buildContext(message, converter.marshal(message));
    getRestClient().delete(rc);
    LOGGER.debug("alerts deleted ");
  }

}
