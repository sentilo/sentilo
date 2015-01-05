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
package org.sentilo.platform.client.core.service.impl;

import org.sentilo.common.rest.RequestParameters;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.domain.CatalogDeleteInputMessage;
import org.sentilo.platform.client.core.domain.CatalogInputMessage;
import org.sentilo.platform.client.core.domain.CatalogOutputMessage;
import org.sentilo.platform.client.core.parser.CatalogAlertMessageConverter;
import org.sentilo.platform.client.core.parser.CatalogMessageConverter;
import org.sentilo.platform.client.core.service.CatalogServiceOperations;
import org.sentilo.platform.client.core.utils.RequestUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

@Service
public class DefaultCatalogServiceOperationsImpl extends AbstractServiceOperationsImpl implements CatalogServiceOperations {

  private final Logger logger = LoggerFactory.getLogger(DefaultCatalogServiceOperationsImpl.class);

  private CatalogMessageConverter converter = new CatalogMessageConverter();
  private CatalogAlertMessageConverter alertConverter = new CatalogAlertMessageConverter();

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#getSensors(org.sentilo.platform
   * .client.core.domain.CatalogInputMessage)
   */
  @Override
  public CatalogOutputMessage getSensors(final CatalogInputMessage message) {
    logger.debug("Retrieving authorized sensors from catalog ");
    final RequestParameters parameters = new RequestParameters();
    if (!CollectionUtils.isEmpty(message.getParameters())) {
      parameters.put(message.getParameters());
    }

    final String response = getRestClient().get(RequestUtils.buildPath(message), parameters, message.getIdentityToken());
    logger.debug("Sensors retrieved ");

    return converter.unmarshall(response);

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
    logger.debug("Registering sensors");
    getRestClient().post(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("Sensors registered ");
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
    logger.debug("Updating sensors");
    getRestClient().put(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("Sensors updated");
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
    logger.debug("Updating components");
    getRestClient().put(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("Components updated");

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
    logger.debug("Deleting provider components/sensors");
    getRestClient().delete(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("Provider components/sensors deleted");
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#getAuthorizedAlerts(org.sentilo
   * .platform.client.core.domain.CatalogAlertInputMessage)
   */
  @Override
  public CatalogAlertOutputMessage getAuthorizedAlerts(final CatalogAlertInputMessage message) {
    logger.debug("Retrieving authorized alerts from catalog ");
    final RequestParameters parameters = new RequestParameters();
    if (!CollectionUtils.isEmpty(message.getParameters())) {
      parameters.put(message.getParameters());
    }

    final String response = getRestClient().get(RequestUtils.buildPath(message), parameters, message.getIdentityToken());
    logger.debug("alerts retrieved ");

    return alertConverter.unmarshall(response);
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#registerAlerts(org.sentilo
   * .platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void registerAlerts(final CatalogAlertInputMessage message) {
    logger.debug("Registering alerts");
    getRestClient().post(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("alerts registered ");
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#updateAlerts(org.sentilo.
   * platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void updateAlerts(final CatalogAlertInputMessage message) {
    logger.debug("Updating alerts");
    getRestClient().put(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("alerts updated ");

  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.platform.client.core.service.CatalogServiceOperations#deleteAlerts(org.sentilo.
   * platform.client.core.domain.CatalogAlertInputMessage)
   */
  public void deleteAlerts(final CatalogAlertInputMessage message) {
    logger.debug("Deleting alerts");
    getRestClient().delete(RequestUtils.buildPath(message), converter.marshall(message), message.getIdentityToken());
    logger.debug("alerts deleted ");

  }

}
