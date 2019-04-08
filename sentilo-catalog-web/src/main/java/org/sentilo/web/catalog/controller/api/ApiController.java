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
import org.sentilo.web.catalog.dto.EntitiesMetadataDTO;
import org.sentilo.web.catalog.exception.NotAllowedActionException;
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
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * API Access to these methods are only granted to users with role ROLE_PLATFORM (@see
 * catalog-security-context.xml).
 */
@Controller
@RequestMapping("/api")
public class ApiController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApiController.class);

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

  @RequestMapping(value = "/entities/permissions", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public Permissions getPermissions() {
    return permissionService.retrievePermissions();
  }

  @RequestMapping(value = "/entities/metadata", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public EntitiesMetadataDTO getEntitiesMetadata() {
    LOGGER.debug("Catalog API: get entities metadata");
    final EntitiesMetadataDTO entitiesMetadata = new EntitiesMetadataDTO();
    entitiesMetadata.addAllApplications(applicationService.findAll());
    entitiesMetadata.addAllProviders(providerService.findAll());
    LOGGER.debug("Catalog API: found {} entities metadata", entitiesMetadata.getEntitiesMetadata().size());
    return entitiesMetadata;
  }

  @RequestMapping(value = "/provider/{providerId}", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogResponseMessage registerSensors(@RequestBody final CatalogInputMessage message, @PathVariable final String providerId) {
    LOGGER.debug("Catalog API: register sensors");
    try {

      final List<Component> components = new ArrayList<Component>();
      final List<Sensor> sensors = new ArrayList<Sensor>();
      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId);

      // The first step must be to validate the <providerId> parameter to verify that actually it
      // represents a provider and not an application.
      checkProviderAccess(providerId);

      // Once the providerId has been validated, the next step would consist of validating the
      // right format and type of each field value
      ApiValidationResults validationResults = validator.validateFieldFormatValues(context);

      // If not exist format errors, the binding process starts
      if (!validationResults.hasErrors()) {
        // Build the list of components or sensors to insert
        components.addAll(ApiConverter.buildComponentsFromCatalogComponents(context));
        sensors.addAll(ApiConverter.buildSensorsFromCatalogSensors(context));

        // Validate that the data to save is valid (from a business point view)
        validationResults = validator.validateSensorsAndComponents(sensors, components, false);
      }

      if (validationResults.hasErrors()) {
        final String errorMessage = "Bad request data. Sensors have not been inserted. Please review the following errors";
        LOGGER.debug("Catalog API: sensors have not been inserted. Found {} errors after validate data. {}", validationResults.getErrorsCount(),
            validationResults.toString());
        return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, validationResults.getErrors());
      } else {
        componentService.insertAll(components);
        sensorService.insertAll(sensors);
        LOGGER.debug("Catalog API: inserted {} components and {} sensors", components.size(), sensors.size());
      }
    } catch (final NotAllowedActionException naae) {
      LOGGER.warn("Rejected operation to register sensors because {} doesn't represents a provider.", providerId);
      return new CatalogResponseMessage(CatalogResponseMessage.FORBIDDEN, SentiloConstants.RESTRICTED_TO_PROVIDERS_ERROR);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error inserting data into database.", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/provider/{providerId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogResponseMessage updateComponentOrSensors(@RequestBody final CatalogInputMessage message, @PathVariable final String providerId) {
    LOGGER.debug("Catalog API: update sensors or components");
    try {
      final List<Component> components = new ArrayList<Component>();
      final List<Sensor> sensors = new ArrayList<Sensor>();

      // The first step must be to validate the <providerId> parameter to verify that actually it
      // represents a provider and not an application.
      checkProviderAccess(providerId);

      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService, providerId, true);

      // 0. Once the providerId has been validated, the next step would consist of validating the
      // right format of each field value
      ApiValidationResults validationResults = validator.validateFieldFormatValues(context);

      // If not exist format errors, the binding process starts
      if (!validationResults.hasErrors()) {
        // 1. Build the list of components or sensors to update
        components.addAll(ApiConverter.buildComponentsFromCatalogComponents(context));
        sensors.addAll(ApiConverter.buildSensorsFromCatalogSensors(context));

        // 2. Validate that the data to save is valid (from a business point view)
        validationResults = validator.validateSensorsAndComponents(sensors, components, true);
      }

      // 3. If there are validation errors, reject the changes and notify the error. Otherwise, we
      // do the update
      if (validationResults.hasErrors()) {
        final String errorMessage = "Bad request data. Resources have not been updated. Please review the following errors";
        LOGGER.debug("Catalog API: resources have not been updated. Found {} errors after validate data. {}", validationResults.getErrorsCount(),
            validationResults.toString());
        return new CatalogResponseMessage(CatalogResponseMessage.BAD_REQUEST, errorMessage, validationResults.getErrors());
      } else {
        componentService.updateAll(components);
        sensorService.updateAll(sensors);
        LOGGER.debug("Catalog API: updated {} components and {} sensors", components.size(), sensors.size());
      }

    } catch (final NotAllowedActionException naae) {
      LOGGER.warn("Rejected operation to register update sensors/components because {} doesn't represents a provider.", providerId);
      return new CatalogResponseMessage(CatalogResponseMessage.FORBIDDEN, SentiloConstants.RESTRICTED_TO_PROVIDERS_ERROR);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error updating data into database. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }
    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/authorized/provider/{entityId}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogResponseMessage getAuthorizedProviders(@PathVariable final String entityId,
      @RequestParam(required = false) final Map<String, String> parameters) {
    LOGGER.debug("Catalog API: getting authorized sensors and providers for entity {} ", entityId);
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
      LOGGER.error("{} - Error searching authorized providers. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    LOGGER.debug("Catalog API: found {}  authorized providers ", authorizedProviders.size());
    return new CatalogResponseMessage(authorizedProviders);
  }

  @RequestMapping(value = "/delete/provider/{providerId}", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogResponseMessage deleteProviderChilds(@RequestBody(required = false) final CatalogDeleteInputMessage message,
      @PathVariable final String providerId) {
    LOGGER.debug("Catalog API: deleting {} resources ", providerId);

    try {
      // The first step must be to validate the <providerId> parameter to verify that actually it
      // represents a provider and not an application.
      checkProviderAccess(providerId);

      if (message == null || SentiloUtils.arrayIsEmpty(message.getSensorsIds()) && SentiloUtils.arrayIsEmpty(message.getComponentsIds())) {
        providerService.deleteChildren(new Provider(providerId));
        LOGGER.debug("Catalog API: deleted all resources");
      } else if (!SentiloUtils.arrayIsEmpty(message.getSensorsIds())) {
        sensorService.deleteSensors(providerId, message.getSensorsIds());
        LOGGER.debug("Catalog API: deleted {} sensors", message.getSensorsIds().length);
      } else if (!SentiloUtils.arrayIsEmpty(message.getComponentsIds())) {
        componentService.deleteComponents(providerId, message.getComponentsIds());
        LOGGER.debug("Catalog API: deleted {} components", message.getComponentsIds().length);
      }
    } catch (final NotAllowedActionException naae) {
      LOGGER.warn("Rejected operation to delete provider's resources because {} doesn't represents a provider.", providerId);
      return new CatalogResponseMessage(CatalogResponseMessage.FORBIDDEN, SentiloConstants.RESTRICTED_TO_PROVIDERS_ERROR);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error deleting childs from provider {} . ", internalErrorCode, providerId, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    return new CatalogResponseMessage();
  }

  @RequestMapping(value = "/location", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public CatalogResponseMessage updateMobileComponentsLocation(@RequestBody final CatalogInputMessage message) {
    LOGGER.debug("Catalog API: updating mobile component locations");

    // message contains a list with N CatalogSensorElement instances. This list is ordered by
    // timestamp.

    try {
      final ApiConverterContext context = new ApiConverterContext(message, sensorService, componentService);
      final List<Component> components = ApiConverter.buildMobileComponentsFromSensorLocationElements(context);

      LOGGER.debug("Catalog API: build {} components that need update its locations ", components.size());
      componentService.updateAll(components);
    } catch (final Exception ex) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_API_ERROR);
      LOGGER.error("{} - Error updating location for sensors. ", internalErrorCode, ex);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      return new CatalogResponseMessage(errorMessage);
    }

    return new CatalogResponseMessage();

  }

  /**
   * Checks that the parameter <code>providerId</code> represents a provider. Only providers could
   * execute POST/PUT/DELETE requests to catalog.
   *
   * @param providerId
   * @return
   */
  private void checkProviderAccess(final String providerId) throws NotAllowedActionException {
    if (providerService.find(new Provider(providerId)) == null) {
      throw new NotAllowedActionException();
    }
  }
}
