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
package org.sentilo.web.catalog.controller.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.AuthorizedProvider;
import org.sentilo.common.domain.CatalogDeleteInputMessage;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.CatalogResponseMessage;
import org.sentilo.common.domain.CatalogSensor;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.converter.ApiConverter;
import org.sentilo.web.catalog.converter.ApiConverterContext;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Permissions;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.CredentialsDTO;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CatalogSensorService;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.PermissionService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.validator.ApiValidationResults;
import org.sentilo.web.catalog.validator.ApiValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * El acceso a los metodos de esta API esta securizado mediante user/password. Solo la plataforma
 * podra invocar a esta API.
 */

@Controller
@RequestMapping("/api")
public class ApiController {

  private final Logger logger = LoggerFactory.getLogger(ApiController.class);

  @Autowired
  private PermissionService permissionService;

  @Autowired
  private ApplicationService applicationService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private SensorService sensorService;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private CatalogSensorService catalogSensorService;

  @Autowired
  private ApiValidator validator;

  @RequestMapping(value = "/permissions", method = RequestMethod.GET)
  @ResponseBody
  public Permissions getPermissions() {
    return permissionService.retrievePermissions();
  }

  @RequestMapping(value = "/credentials", method = RequestMethod.GET)
  @ResponseBody
  public CredentialsDTO getAuthorizations() {
    logger.debug("Catalog API: get credentials");
    final CredentialsDTO credentials = new CredentialsDTO();
    credentials.addAllApplications(applicationService.findAll());
    credentials.addAllProviders(providerService.findAll());
    logger.debug("Catalog API: found {} credentials", credentials.getCredentials().size());
    return credentials;
  }

  @RequestMapping(value = "/provider/{providerId}", method = RequestMethod.POST)
  @ResponseBody
  public CatalogResponseMessage registerSensors(@RequestBody final CatalogInputMessage message, @PathVariable final String providerId) {
    logger.debug("Catalog API: register sensors");
    try {
      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId);

      // The sensors's register must follows the steps below:
      // 1. To insert components, related to the sensors, that doesn't exists yet.
      // 2. To insert sensors and associated everyone with the pertinent component.
      final List<Component> components = ApiConverter.buildComponentsFromCatalogComponents(context);
      final List<Sensor> sensors = ApiConverter.buildSensorsFromCatalogSensors(context);

      final ApiValidationResults validationResults = validator.validateSensorsAndComponents(sensors, components, false);

      if (validationResults.hasErrors()) {
        final String errorMessage = "Bad request data. Sensors have not been inserted. Please review the following errors";
        logger.debug("Catalog API: sensors have not been inserted. Found {} errors after validate data. {}", validationResults.getErrorsCount(),
            validationResults.toString());
        return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, validationResults.getErrors());
      } else {
        componentService.insertAll(components);
        sensorService.insertAll(sensors);
        logger.debug("Catalog API: inserted {} components and {} sensors", components.size(), sensors.size());
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      logger.error("{} - Error inserting data into database.", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/provider/{providerId}", method = RequestMethod.PUT)
  @ResponseBody
  public CatalogResponseMessage updateComponentOrSensors(@RequestBody final CatalogInputMessage message, @PathVariable final String providerId) {
    logger.debug("Catalog API: update sensors or components");
    try {
      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId, true);
      // 1. Build the list of components or sensors to update
      final List<Component> components = ApiConverter.buildComponentsFromCatalogComponents(context);
      final List<Sensor> sensors = ApiConverter.buildSensorsFromCatalogSensors(context);

      // 2. Validate that the data to save is valid
      final ApiValidationResults validationResults = validator.validateSensorsAndComponents(sensors, components, true);

      // 3. If there are validation errors, reject the changes and notify the error. Otherwise, we
      // do the update
      if (validationResults.hasErrors()) {
        final String errorMessage = "Bad request data. Resources have not been updated. Please review the following errors";
        logger.debug("Catalog API: resources have not been updated. Found {} errors after validate data. {}", validationResults.getErrorsCount(),
            validationResults.toString());
        return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, validationResults.getErrors());
      } else {
        componentService.updateAll(components);
        sensorService.updateAll(sensors);
        logger.debug("Catalog API: updated {} components and {} sensors", components.size(), sensors.size());
      }

    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      logger.error("{} - Error updating data into database. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }
    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/authorized/provider/{entityId}", method = RequestMethod.GET)
  @ResponseBody
  public CatalogResponseMessage getAuthorizedProviders(@PathVariable final String entityId,
      @RequestParam(required = false) final Map<String, String> parameters) {
    logger.debug("Catalog API: getting authorized sensors and providers for entity {} ", entityId);
    final List<AuthorizedProvider> authorizedProviders = new ArrayList<AuthorizedProvider>();
    try {
      final List<Permission> permissions = permissionService.getActivePermissions(entityId);

      // For every permission object, we define a new AuthorizedProvider object with its list of
      // sensors.
      for (final Permission permission : permissions) {
        final List<CatalogSensor> catalogSensors = catalogSensorService.getSensorsByProvider(permission.getTarget(), parameters);
        if (!CollectionUtils.isEmpty(catalogSensors)) {
          authorizedProviders.add(new AuthorizedProvider(permission.getTarget(), permission.getType().toString(), catalogSensors));
        }
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      logger.error("{} - Error searching authorized providers. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    logger.debug("Catalog API: found {}  authorized providers ", authorizedProviders.size());
    return new CatalogResponseMessage(authorizedProviders);
  }

  @RequestMapping(value = "/delete/provider/{providerId}", method = RequestMethod.PUT)
  @ResponseBody
  public CatalogResponseMessage deleteProviderChilds(@RequestBody(required = false) final CatalogDeleteInputMessage message,
      @PathVariable final String providerId) {
    logger.debug("Catalog API: deleting {} resources ", providerId);
    try {
      if (message == null || (SentiloUtils.arrayIsEmpty(message.getSensorsIds()) && SentiloUtils.arrayIsEmpty(message.getComponentsIds()))) {
        providerService.deleteChilds(new Provider(providerId));
        logger.debug("Catalog API: deleted all resources");
      } else if (!SentiloUtils.arrayIsEmpty(message.getSensorsIds())) {
        sensorService.deleteSensors(message.getSensorsIds());
        logger.debug("Catalog API: deleted {} sensors", message.getSensorsIds().length);
      } else if (!SentiloUtils.arrayIsEmpty(message.getComponentsIds())) {
        componentService.deleteComponents(message.getComponentsIds());
        logger.debug("Catalog API: deleted {} components", message.getComponentsIds().length);
      }
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      logger.error("{} - Error deleting childs from provider {} . ", internalErrorCode, providerId, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/location", method = RequestMethod.PUT)
  @ResponseBody
  public CatalogResponseMessage updateMobileComponentsLocation(@RequestBody final CatalogInputMessage message) {
    logger.debug("Catalog API: updating mobile component locations");

    // message contains a list with N CatalogSensorElement instances. This list is ordered by
    // timestamp, and not contains duplicate objects.

    try {
      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService);
      final List<Component> components = ApiConverter.buildMobileComponentsFromSensorLocationElements(context);
      logger.debug("Catalog API: build {} components that need update its locations ", components.size());
      componentService.updateAll(components);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      logger.error("{} - Error updating location for sensors. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);

      // TODO Mikel: Pensar en retornar un mensaje que permita recuperarse del error o reintentar la
      // llamada desde el lado servidor.
      // Esta API sólo es invocada por el servidor PubSub, por lo que la respuesta no debe ser tan
      // amigable y si contener
      // la información necesaria para recuperarse del problema (por ejemplo, retornar ids de
      // recursos problematicos, ... )
    }

    return new CatalogResponseMessage();

  }

}
