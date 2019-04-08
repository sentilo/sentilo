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
package org.sentilo.web.catalog.converter;

import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.validator.ApiValidationResults;

public class ApiAlertConverterContext {

  private final CatalogAlertInputMessage message;
  private final boolean isUpdateAction;
  private final ApplicationService applicationService;
  private final ProviderService providerService;
  private final AlertService alertService;

  private final ApiValidationResults results;

  public ApiAlertConverterContext(final CatalogAlertInputMessage message, final String entityId, final ApplicationService applicationService,
      final ProviderService providerService, final AlertService alertService, final boolean isUpdateAction) {
    super();
    this.message = message;
    this.isUpdateAction = isUpdateAction;
    this.applicationService = applicationService;
    this.providerService = providerService;
    this.alertService = alertService;
    this.message.setEntityId(entityId);

    results = new ApiValidationResults();
  }

  public ApiAlertConverterContext(final CatalogAlertInputMessage message, final String entityId, final ApplicationService applicationService,
      final ProviderService providerService, final AlertService alertService) {
    this(message, entityId, applicationService, providerService, alertService, false);
  }

  public CatalogAlertInputMessage getMessage() {
    return message;
  }

  public boolean isUpdateAction() {
    return isUpdateAction;
  }

  public ApplicationService getApplicationService() {
    return applicationService;
  }

  public ProviderService getProviderService() {
    return providerService;
  }

  public ApiValidationResults getResults() {
    return results;
  }

  public AlertService getAlertService() {
    return alertService;
  }
}
