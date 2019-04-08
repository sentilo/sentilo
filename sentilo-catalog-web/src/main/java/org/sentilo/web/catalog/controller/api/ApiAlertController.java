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
package org.sentilo.web.catalog.controller.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.domain.CatalogAlertInputMessage;
import org.sentilo.common.domain.CatalogAlertResponseMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.converter.ApiAlertConverter;
import org.sentilo.web.catalog.converter.ApiAlertConverterContext;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.AlertService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.validator.ApiAlertValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/api/alert")
public class ApiAlertController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApiAlertController.class);

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private AlertService alertService;

  @Autowired
  private ApiAlertValidator validator;

  @Autowired
  private CatalogUserDetailsService userDetailsService;

  @RequestMapping(value = "/entity/{entityId}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogAlertResponseMessage getAuthorizedAlerts(@PathVariable final String entityId,
      @RequestParam(required = false) final Map<String, Object> parameters) {
    // Retornamos un listado con la información de todas las alertas a las cuales se puede suscribir
    // la entidad que hace la peticion. Es decir, todas las alertas que tienen como entidad
    // propietaria una entidad para la cual la entidad que hace la peticion (aka entidad
    // origen)tiene permiso de lectura.
    LOGGER.debug("Catalog Alert API: getting authorized alerts. Operation invoked by entity {} ", entityId);
    CatalogAlertResponseMessage response = null;
    try {
      // 1. Recuperamos la lista de permisos de la entidad origen.
      final List<Permission> permissions = permissionService.getActivePermissions(entityId);

      // 2. Recuperamos la lista de alertas asociadas a estas entidades.
      final List<String> entities = new ArrayList<String>();
      for (final Permission permission : permissions) {
        entities.add(permission.getTarget());
      }

      final List<Alert> alerts = alertService.getAlertsByEntities(entities, parameters);
      final List<CatalogAlert> catalogAlerts = ApiAlertConverter.convertToCatalogAlertList(alerts);

      LOGGER.debug("Catalog Alert API: found {}  authorized alerts ", alerts.size());
      response = new CatalogAlertResponseMessage();
      response.setAlerts(catalogAlerts);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_ALERT_API_ERROR);
      LOGGER.error("{} - Error searching authorized alerts for entity {}. ", internalErrorCode, entityId, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogAlertResponseMessage(errorMessage);
    }

    return response;
  }

  @RequestMapping(value = "/entity/{entityId}", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogAlertResponseMessage createAlerts(@PathVariable final String entityId, @RequestBody final CatalogAlertInputMessage message) {
    LOGGER.debug("Catalog alert API: registering new alerts. Operation invoked by entity {} ", entityId);
    final CatalogUserDetails catalogUser = userDetailsService.getCatalogUserDetails();
    try {
      final ApiAlertConverterContext context = new ApiAlertConverterContext(message, entityId, applicationService, providerService, alertService);

      final List<Alert> alerts = ApiAlertConverter.buildAlertsFromCatalogAlerts(context, catalogUser.getUsername());

      if (!context.getResults().hasErrors()) {
        validator.validate(alerts, context.getResults(), false);
      }

      if (context.getResults().hasErrors()) {
        final String errorMessage = "Bad request data. Alerts have not been inserted. Please review the following errors";
        LOGGER.debug("Catalog alert API: alerts have not been inserted. Found {} errors after validate data. {}",
            context.getResults().getErrorsCount(), context.getResults().toString());
        return new CatalogAlertResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, context.getResults().getErrors());
      } else {
        alertService.insertAll(alerts);
        LOGGER.debug("Catalog alert API: inserted {} alerts", alerts.size());
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error inserting alerts into database. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogAlertResponseMessage(errorMessage);
    }

    return new CatalogAlertResponseMessage();
  }

  @RequestMapping(value = "/entity/{entityId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogAlertResponseMessage updateAlerts(@PathVariable final String entityId, @RequestBody final CatalogAlertInputMessage message) {
    LOGGER.debug("Catalog alert API: updating alerts. Operation invoked by entity {} ", entityId);
    final CatalogUserDetails catalogUser = userDetailsService.getCatalogUserDetails();
    try {
      final ApiAlertConverterContext context =
          new ApiAlertConverterContext(message, entityId, applicationService, providerService, alertService, true);

      final List<Alert> alerts = ApiAlertConverter.buildAlertsFromCatalogAlerts(context, catalogUser.getUsername());

      if (!context.getResults().hasErrors()) {
        validator.validate(alerts, context.getResults(), true);
      }

      if (context.getResults().hasErrors()) {
        final String errorMessage = "Bad request data. Alerts have not been updated. Please review the following errors";
        LOGGER.debug("Catalog alert API: alerts have not been updated. Found {} errors after validate data. {}",
            context.getResults().getErrorsCount(), context.getResults().toString());
        return new CatalogAlertResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, context.getResults().getErrors());
      } else {
        alertService.updateAll(alerts);
        LOGGER.debug("Catalog alert API: updated {} alerts", alerts.size());
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error updating alerts into database. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogAlertResponseMessage(errorMessage);
    }

    return new CatalogAlertResponseMessage();
  }

  @RequestMapping(value = "/entity/{entityId}/delete", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogAlertResponseMessage deleteAlerts(@PathVariable final String entityId,
      @RequestBody(required = false) final CatalogAlertInputMessage message) {
    // Solo se pueden borrar alarmas externas de las cuales se es el propietario.
    // Las alarmas internas solo se pueden borrar via la web.

    LOGGER.debug("Catalog alert API: deleting alerts. Operation invoked by entity {} ", entityId);
    try {
      if (message == null || SentiloUtils.arrayIsEmpty(message.getAlertsIds())) {
        alertService.deleteOwnAlerts(entityId);
        LOGGER.debug("Catalog alert API: deleted all alerts from entity {}", entityId);
      } else if (!SentiloUtils.arrayIsEmpty(message.getAlertsIds())) {
        alertService.deleteOwnAlerts(message.getAlertsIds(), entityId);
        LOGGER.debug("Catalog alert API: deleted {} alerts", message.getAlertsIds().length);
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error deleting alerts. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogAlertResponseMessage(errorMessage);
    }
    return new CatalogAlertResponseMessage();
  }
}
