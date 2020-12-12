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
package org.sentilo.web.catalog.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.sentilo.common.domain.TechnicalDetails;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.ActiveSubscription;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.AlertRule;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.FederationConfig;
import org.sentilo.web.catalog.domain.LngLat;
import org.sentilo.web.catalog.domain.Permission;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.TenantPermission;
import org.sentilo.web.catalog.domain.TenantResource;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.domain.VisualConfiguration;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.SensorSubstateService;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.util.StringUtils;

public class ExcelGeneratorUtils {

  private enum ListType {
    provider, application, component, sensor, sensortype, componenttype, permission, grant, user, alert, alertrule, tenant, tenantpermission, activesubscription, federationconfig;
  }

  public static List<String> getColumnsNames(final String listTypeNAme) {

    final ListType listType = ListType.valueOf(listTypeNAme);

    switch (listType) {
      case provider:
        return getProviderExcelColumnNames();
      case application:
        return getApplicationExcelColumnNames();
      case component:
        return getComponentExcelColumnNames();
      case sensor:
        return getSensorExcelColumnNames();
      case sensortype:
        return getSensorTypesExcelColumnNames();
      case componenttype:
        return getComponentTypeExcelColumnNames();
      case grant:
        return getTenantPermissionsExcelColumnNames();
      case user:
        return getUserExcelColumnNames();
      case alert:
        return getAlertExcelColumnNames();
      case alertrule:
        return getAlertRuleExcelColumnNames();
      case tenant:
        return getTenantExcelColumnNames();
      case permission:
        return getPermissionsExcelColumnNames();
      case tenantpermission:
        return getTenantPermissionsExcelColumnNames();
      case activesubscription:
        return getActiveSubscriptionExcelColumnNames();
      case federationconfig:
        return getFederationConfigExcelColumnNames();
      default:
        return Collections.emptyList();
    }

  }

  public static List<String> getAlertExcelRowsData(final Alert alert, final MessageSource messageSource,
      final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();

    row.add(alert.getId());
    row.add(alert.getName());
    row.add(alert.getDescription());
    row.add(FormatUtils.label(messageSource.getMessage("alert.type." + alert.getType().toString(), null, LocaleContextHolder.getLocale())));
    row.add(FormatUtils.formatAlertTriggerColumn(alert));
    row.add(alert.getExpression());
    row.add(String.valueOf(alert.isActive()));
    row.add(alert.getProviderId());
    row.add(alert.getComponentId());
    row.add(alert.getSensorId());
    row.add(alert.getApplicationId());

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(alert, row);
    }

    addAuditInfo(alert, row, localDateFormatter);

    return row;
  }

  private static List<String> getAlertExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.TYPE_PROP);
    listColumnNames.add(Constants.TRIGGER_PROP);
    listColumnNames.add(Constants.EXPRESSION_PROP);
    listColumnNames.add(Constants.ACTIVE_PROP);
    listColumnNames.add(Constants.PROVIDER_ID_PROP);
    listColumnNames.add(Constants.COMPONENT_ID_PROP);
    listColumnNames.add(Constants.SENSOR_ID_PROP);
    listColumnNames.add(Constants.APPLICATION_ID_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getAlertRuleExcelRowsData(final AlertRule alertRule, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();

    row.add(alertRule.getId());
    row.add(alertRule.getName());
    row.add(alertRule.getDescription());
    row.add(alertRule.getProviderId());
    row.add(alertRule.getComponentType());
    row.add(alertRule.getSensorType());
    row.add(FormatUtils.formatAlertRuleTriggerColumn(alertRule));
    row.add(alertRule.getExpression());
    row.add(String.valueOf(alertRule.getTotalSensors()));
    row.add(String.valueOf(alertRule.getGeneratedAlerts()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(alertRule, row);
    }

    addAuditInfo(alertRule, row, localDateFormatter);

    return row;
  }

  private static List<String> getAlertRuleExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.PROVIDER_ID_PROP);
    listColumnNames.add(Constants.COMPONENT_TYPE_PROP);
    listColumnNames.add(Constants.SENSOR_TYPE_PROP);
    listColumnNames.add(Constants.TRIGGER_PROP);
    listColumnNames.add(Constants.EXPRESSION_PROP);
    listColumnNames.add(Constants.TOTAL_SENSORS_PROP);
    listColumnNames.add(Constants.GENERATED_ALERTS_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getApplicationExcelRowsData(final Application application, final LocalDateFormatter localDateFormatter,
      final CatalogUserDetails userDetails) {

    final boolean isAdmin = userDetails.isAdminUser();

    final List<String> row = new ArrayList<String>();
    row.add(application.getId());
    row.add(application.getName());
    row.add(application.getDescription());
    row.add(isAdmin ? application.getToken() : Constants.HIDDEN_TOKEN_STR);
    row.add(application.getEmail());

    final List<String> authProvIds = new ArrayList<String>();
    for (final Provider provider : application.getAuthorizedProviders()) {
      authProvIds.add(provider.getId());
    }

    row.add(CatalogUtils.collectionToString(authProvIds));
    row.add(String.valueOf(application.isRestHttps()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(application, row);
    }

    addAuditInfo(application, row, localDateFormatter);

    return row;
  }

  private static List<String> getApplicationExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.TOKEN_PROP);
    listColumnNames.add(Constants.EMAIL_PROP);
    listColumnNames.add(Constants.AUTHORIZED_PROVIDERS_PROP);
    listColumnNames.add(Constants.REST_HTTPS_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getPermissionsExcelRowsData(final Permission permission, final MessageSource messageSource,
      final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();
    row.add(permission.getSource());
    row.add(permission.getTarget());
    row.add(messageSource.getMessage(Constants.PERMISSION_PROP + "." + permission.getType().toString(), null, LocaleContextHolder.getLocale()));

    addAuditInfo(permission, row, localDateFormatter);
    return row;
  }

  private static List<String> getPermissionsExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.SOURCE_PROP);
    listColumnNames.add(Constants.TARGET_PROP);
    listColumnNames.add(Constants.TYPE_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);
    return listColumnNames;
  }

  public static List<String> getComponentExcelRowsData(final Component component, final MessageSource messageSource,
      final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();

    row.add(component.getId());
    row.add(component.getName());
    row.add(component.getDescription());
    row.add(component.getProviderId());
    row.add(buildLocationCell(component));
    row.add(String.valueOf(component.getPublicAccess()));

    if (component.isMobileComponent()) {
      row.add(FormatUtils.label(messageSource.getMessage("mobile", null, LocaleContextHolder.getLocale())));
    } else {
      row.add(FormatUtils.label(messageSource.getMessage("static", null, LocaleContextHolder.getLocale())));
    }

    row.add(component.getParentId());
    row.add(component.getTags());
    row.add(FormatUtils.label(component.getComponentType()));
    row.add(component.getPhotoUrl());

    // Additional info map
    row.add(getAdditionalInfo(component.getAdditionalInfo()));

    // TechnicalDetails
    row.addAll(addTechnicalDetailsToRow(component.getTechnicalDetails()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(component, row);
      row.add(CatalogUtils.collectionToString(component.getTenantsMapVisible()));
    }

    addAuditInfo(component, row, localDateFormatter);

    return row;
  }

  private static String buildLocationCell(final Component component) {
    final StringBuilder sbL = new StringBuilder();
    if (component.getLocation() != null) {
      if (component.getLocation().getCoordinates() != null && component.getLocation().getCoordinates().length > 0) {
        for (final LngLat coord : component.getLocation().getCoordinates()) {
          if (sbL.length() > 0) {
            sbL.append("\n");
          }
          sbL.append(coord.getLatitude() + ", " + coord.getLongitude());
        }
      } else if (component.getLocation().getCentroid() != null && component.getLocation().getCoordinates().length == 2) {
        sbL.append(component.getLocation().getCoordinates()[1] + ", " + component.getLocation().getCoordinates()[0]);
      }
    }

    return sbL.toString();
  }

  private static List<String> getComponentExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.PROVIDER_ID_PROP);
    listColumnNames.add(Constants.LOCATION_PROP);
    listColumnNames.add(Constants.PUBLIC_ACCESS_PROP);
    listColumnNames.add(Constants.MOBILE_PROP);
    listColumnNames.add(Constants.PARENT_ID_PROP);
    listColumnNames.add(Constants.TAGS_PROP);
    listColumnNames.add(Constants.COMPONENT_TYPE_PROP);
    listColumnNames.add(Constants.PHOTO_URL_PROP);
    listColumnNames.add(Constants.ADDITIONAL_INFO_PROP);
    listColumnNames.addAll(addTechnicalDetailsColumnNames());

    // Hide the routePointList field, because it must be not exported to excel
    // listColumnNames.add(Constants.ROUTE_POINT_LIST_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
      listColumnNames.add(Constants.TENANTS_MAP_VISIBLE_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getComponentTypesExcelRowsData(final ComponentType componentType, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();
    row.add(componentType.getId());
    row.add(componentType.getName());
    row.add(componentType.getDescription());
    row.add(componentType.getIcon());
    row.add(componentType.getPhotoUrl());
    addAuditInfo(componentType, row, localDateFormatter);
    return row;
  }

  private static List<String> getComponentTypeExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.ICON_PROP);
    listColumnNames.add(Constants.PHOTO_URL_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);
    return listColumnNames;
  }

  public static List<String> getProviderExcelRowsData(final Provider provider, final LocalDateFormatter localDateFormatter,
      final CatalogUserDetails userDetails) {

    final boolean isAdmin = userDetails.isAdminUser();

    final List<String> row = new ArrayList<String>();
    row.add(provider.getId());
    row.add(provider.getName());
    row.add(provider.getDescription());
    row.add(isAdmin ? provider.getToken() : Constants.HIDDEN_TOKEN_STR);
    row.add(provider.getContact().getName());
    row.add(provider.getContact().getEmail());
    row.add(String.valueOf(provider.isRestHttps()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(provider, row);
    }

    addAuditInfo(provider, row, localDateFormatter);

    return row;
  }

  private static List<String> getProviderExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.TOKEN_PROP);
    listColumnNames.add(Constants.CONTACT_NAME_PROP);
    listColumnNames.add(Constants.CONTACT_EMAIL_PROP);
    listColumnNames.add(Constants.REST_HTTPS_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getSensorExcelRowsData(final Sensor sensor, final LocalDateFormatter localDateFormatter,
      final SensorSubstateService sensorSubstateService) {
    final List<String> row = new ArrayList<String>();

    row.add(sensor.getId());
    row.add(sensor.getSensorId());
    row.add(sensor.getProviderId());
    row.add(sensor.getComponentId());
    row.add(sensor.getDescription());
    row.add(sensor.getDataType().name());
    row.add(FormatUtils.label(sensor.getType()));
    row.add(sensor.getUnit());
    row.add(sensor.getValidTime());
    row.add(sensor.getTimeZone());
    row.add(sensor.getTags());
    row.add(String.valueOf(sensor.getPublicAccess()));
    // Hide the metadata field, because it is not longer used
    // row.add(sensor.getMetaData());
    row.add(sensor.getState().toString());
    row.add(StringUtils.hasText(sensor.getSubstate()) ? FormatUtils.substateStyleColumn(sensor, sensorSubstateService) : null);

    // Additional info map
    row.add(getAdditionalInfo(sensor.getAdditionalInfo()));

    // TechnicalDetails
    row.addAll(addTechnicalDetailsToRow(sensor.getTechnicalDetails()));

    // VisualConfiguration
    row.add(getVisualConfiguration(sensor.getVisualConfiguration()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(sensor, row);
    }

    addAuditInfo(sensor, row, localDateFormatter);

    return row;
  }

  private static List<String> getSensorExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();

    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.SENSOR_ID_PROP);
    listColumnNames.add(Constants.PROVIDER_ID_PROP);
    listColumnNames.add(Constants.COMPONENT_ID_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.DATA_TYPE_PROP);
    listColumnNames.add(Constants.TYPE_PROP);
    listColumnNames.add(Constants.UNIT_PROP);
    listColumnNames.add(Constants.VALID_TIME_PROP);
    listColumnNames.add(Constants.TIME_ZONE_PROP);
    listColumnNames.add(Constants.TAGS_PROP);
    listColumnNames.add(Constants.PUBLIC_ACCESS_PROP);
    // Hide the metadata field, because it is not longer used
    // listColumnNames.add(Constants.METADATA_PROP);
    listColumnNames.add(Constants.STATE_PROP);
    listColumnNames.add(Constants.SUBSTATE_PROP);
    listColumnNames.add(Constants.ADDITIONAL_INFO_PROP);
    listColumnNames.addAll(addTechnicalDetailsColumnNames());
    listColumnNames.add(Constants.VISUAL_CONFIGURATION_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getSensorTypesExcelRowsData(final SensorType sensorType, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();
    row.add(sensorType.getId());
    row.add(sensorType.getName());
    row.add(sensorType.getDescription());
    addAuditInfo(sensorType, row, localDateFormatter);
    return row;
  }

  private static List<String> getSensorTypesExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getTenantExcelRowsData(final Tenant tenant, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new LinkedList<String>();

    row.add(tenant.getId());
    row.add(tenant.getName());
    row.add(tenant.getDescription());
    row.add(tenant.getContactName());
    row.add(tenant.getContactEmail());
    row.add(String.valueOf(tenant.getIsPublic() != null ? tenant.getIsPublic() : Boolean.FALSE));
    row.add(String.valueOf(tenant.getIsDefault()));

    final Map<String, String> mapParamsMap = new HashMap<String, String>();
    if (tenant.getMapParams() != null) {
      mapParamsMap.put(Constants.MAPPARAMS_ZOOMLEVEL_PROP, String.valueOf(tenant.getMapParams().getZoomLevel()));

      if (tenant.getMapParams().getCenter() != null) {
        mapParamsMap.put(Constants.MAPPARAMS_CENTER_PROP,
            String.valueOf(tenant.getMapParams().getCenter().getLatitude()) + "," + String.valueOf(tenant.getMapParams().getCenter().getLongitude()));
      }

      if (StringUtils.hasText(tenant.getMapParams().getBgColor())) {
        mapParamsMap.put(Constants.MAPPARAMS_BGCOLOR_PROP, tenant.getMapParams().getBgColor());
      }
    }
    row.add(CatalogUtils.mapToString(mapParamsMap));

    // VisualConfiguration
    row.add(getVisualConfiguration(tenant.getVisualConfiguration()));

    addAuditInfo(tenant, row, localDateFormatter);

    return row;
  }

  private static List<String> getTenantExcelColumnNames() {
    final List<String> listColumnNames = new LinkedList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.CONTACT_NAME_PROP);
    listColumnNames.add(Constants.CONTACT_EMAIL_PROP);
    listColumnNames.add(Constants.IS_PUBLIC_PROP);
    listColumnNames.add(Constants.IS_DEFAULT_PROP);
    listColumnNames.add(Constants.MAPPARAMS_PROP);
    listColumnNames.add(Constants.VISUAL_CONFIGURATION_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  public static List<String> getTenantPermissionsExcelRowsData(final TenantPermission permission, final MessageSource messageSource,
      final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();

    // Assume that TenantContext exists
    final boolean isToPermission = permission.getSource().equals(TenantUtils.getCurrentTenant());

    if (isToPermission) {
      row.add(permission.getTarget());
      row.add(permission.getSource());
    } else {
      row.add(permission.getSource());
      row.add(permission.getTarget());
    }

    row.add(messageSource.getMessage(Constants.PERMISSION_PROP + "." + permission.getType(), null, LocaleContextHolder.getLocale()));
    row.add(permission.getEntity());
    row.add(messageSource.getMessage(String.valueOf(permission.getVisible()), null, LocaleContextHolder.getLocale()));

    addAuditInfo(permission, row, localDateFormatter);

    return row;
  }

  private static List<String> getTenantPermissionsExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.SOURCE_PROP);
    listColumnNames.add(Constants.TARGET_PROP);
    listColumnNames.add(Constants.TYPE_PROP);
    listColumnNames.add(Constants.ENTITY_PROP);
    listColumnNames.add(Constants.VISIBLE_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);
    return listColumnNames;
  }

  public static List<String> getUserExcelRowsData(final User user, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new ArrayList<String>();
    row.add(user.getUserName());
    row.add(user.getName());
    row.add(user.getEmail());
    row.add(user.getDescription());
    row.add(String.valueOf(user.isActive()));

    final List<String> roleNames = new ArrayList<String>();
    for (final Role role : user.getRoles()) {
      roleNames.add(role.name());
    }
    row.add(CatalogUtils.collectionToString(roleNames));

    // VisualConfiguration
    row.add(getVisualConfiguration(user.getVisualConfiguration()));

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      addTenantInfo(user, row);
    }

    addAuditInfo(user, row, localDateFormatter);

    return row;
  }

  public static List<String> getActiveSubscriptionsExcelRowsData(final ActiveSubscription activeSubscription,
      final LocalDateFormatter localDateFormatter, final CatalogUserDetails userDetails) {
    final List<String> row = new ArrayList<String>();
    row.add(activeSubscription.getEntityId());
    row.add(activeSubscription.getEntityType().name());
    row.add(activeSubscription.getSubscriptionType());
    if (activeSubscription.getProvider() != null && activeSubscription.getProvider().endsWith("*")) {
      row.add(activeSubscription.getProvider().substring(0, activeSubscription.getProvider().length() - 1));
    } else {
      row.add(activeSubscription.getProvider());
    }
    row.add(activeSubscription.getSensor());
    row.add(activeSubscription.getEndpoint());
    row.add(String.valueOf(activeSubscription.getMaxRetries()));
    row.add(String.valueOf(activeSubscription.getRetryDelay()));
    return row;
  }

  public static List<String> getFederationConfigExcelRowsData(final FederationConfig resource, final LocalDateFormatter localDateFormatter) {
    final List<String> row = new LinkedList<String>();

    row.add(resource.getId());
    row.add(resource.getName());
    row.add(resource.getDescription());
    row.add(resource.getAppClientName());
    row.add(resource.getAppClientToken());
    row.add(resource.getSourceEndpoint());
    row.add(resource.getSourceContactName());
    row.add(resource.getSourceContactMail());
    row.add(localDateFormatter.printAsLocalTime(resource.getLastSyncTime(), Constants.DATETIME_FORMAT));

    addAuditInfo(resource, row, localDateFormatter);

    return row;
  }

  private static List<String> getFederationConfigExcelColumnNames() {
    final List<String> listColumnNames = new LinkedList<String>();
    listColumnNames.add(Constants.ID_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.APP_CLIENT_NAME_PROP);
    listColumnNames.add(Constants.APP_CLIENT_TOKEN_PROP);
    listColumnNames.add(Constants.FEDERATION_SERVER_API_ENDPOINT_PROP);
    listColumnNames.add(Constants.CONTACT_NAME_PROP);
    listColumnNames.add(Constants.CONTACT_EMAIL_PROP);
    listColumnNames.add(Constants.FEDERATION_SERVER_LAST_SYNC_PROP);
    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  private static List<String> getUserExcelColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.USER_NAME_PROP);
    listColumnNames.add(Constants.NAME_PROP);
    listColumnNames.add(Constants.EMAIL_PROP);
    listColumnNames.add(Constants.DESCRIPTION_PROP);
    listColumnNames.add(Constants.ACTIVE_PROP);
    listColumnNames.add(Constants.ROLES_PROP);
    listColumnNames.add(Constants.VISUAL_CONFIGURATION_PROP);

    // Only for multitenant version
    if (TenantContextHolder.isEnabled()) {
      listColumnNames.add(Constants.TENANT_ID_PROP);
      listColumnNames.add(Constants.TENANTS_AUTH_PROP);
    }

    listColumnNames.add(Constants.CREATED_AT_PROP);
    listColumnNames.add(Constants.CREATED_BY_PROP);
    listColumnNames.add(Constants.UPDATED_AT_PROP);
    listColumnNames.add(Constants.UPDATED_BY_PROP);

    return listColumnNames;
  }

  private static List<String> getActiveSubscriptionExcelColumnNames() {
    final List<String> listColumnNames = new LinkedList<String>();

    listColumnNames.add(Constants.ENTITY_ID_PROP);
    listColumnNames.add(Constants.ENTITY_TYPE_PROP);
    listColumnNames.add(Constants.SUBSCRIPTION_TYPE_PROP);
    listColumnNames.add(Constants.PROVIDER_PROP);
    listColumnNames.add(Constants.SENSOR_PROP);
    listColumnNames.add(Constants.ENDPOINT_PROP);
    listColumnNames.add(Constants.MAX_RETRIES_PROP);
    listColumnNames.add(Constants.RETRY_DELAY_PROP);

    return listColumnNames;
  }

  private static List<String> addTechnicalDetailsToRow(final TechnicalDetails technicalDetails) {
    final List<String> row = new ArrayList<String>();
    if (technicalDetails != null) {
      row.add(StringUtils.hasText(technicalDetails.getProducer()) ? technicalDetails.getProducer() : "");
      row.add(StringUtils.hasText(technicalDetails.getModel()) ? technicalDetails.getModel() : "");
      row.add(StringUtils.hasText(technicalDetails.getSerialNumber()) ? technicalDetails.getSerialNumber() : "");
      row.add(StringUtils.hasText(technicalDetails.getMacAddress()) ? technicalDetails.getMacAddress() : "");
      row.add(StringUtils.hasText(technicalDetails.getEnergy()) ? technicalDetails.getEnergy() : "");
      row.add(StringUtils.hasText(technicalDetails.getConnectivity()) ? technicalDetails.getConnectivity() : "");
    } else {
      row.add("");
      row.add("");
      row.add("");
      row.add("");
      row.add("");
      row.add("");
    }
    return row;
  }

  private static List<String> addTechnicalDetailsColumnNames() {
    final List<String> listColumnNames = new ArrayList<String>();
    listColumnNames.add(Constants.TECHNICAL_DETAILS_PRODUCER_PROP);
    listColumnNames.add(Constants.TECHNICAL_DETAILS_MODEL_PROP);
    listColumnNames.add(Constants.TECHNICAL_DETAILS_SERIAL_PROP);
    listColumnNames.add(Constants.TECHNICAL_DETAILS_MAC_PROP);
    listColumnNames.add(Constants.TECHNICAL_DETAILS_ENERGY_PROP);
    listColumnNames.add(Constants.TECHNICAL_DETAILS_CONN_PROP);
    return listColumnNames;
  }

  private static String getAdditionalInfo(final Map<String, String> additionalInfo) {
    return CatalogUtils.mapToString(additionalInfo);
  }

  private static String getVisualConfiguration(final VisualConfiguration visualConfiguration) {
    final Map<String, String> keyValues = new HashMap<String, String>();
    if (visualConfiguration != null) {
      if (StringUtils.hasText(visualConfiguration.getTimeZone())) {
        keyValues.put(Constants.VISUAL_CONFIGURATION_TIMEZONE_PROP, visualConfiguration.getTimeZone());
      }

      if (StringUtils.hasText(visualConfiguration.getDateFormatPattern())) {
        keyValues.put(Constants.VISUAL_CONFIGURATION_DATFEORMATPATTERN_PROP, visualConfiguration.getDateFormatPattern());
      }

      if (visualConfiguration.getChartVisiblePointsNumber() != null) {
        keyValues.put(Constants.VISUAL_CONFIGURATION_CHARTVISIBLEOBSN_PROP, Integer.toString(visualConfiguration.getChartVisiblePointsNumber()));
      }
    }
    return CatalogUtils.mapToString(keyValues);
  }

  private static void addTenantInfo(final TenantResource resource, final List<String> row) {
    row.add(resource.getTenantId());
    row.add(CatalogUtils.collectionToString(resource.getTenantsAuth()));
  }

  private static void addAuditInfo(final CatalogDocument resource, final List<String> row, final LocalDateFormatter localDateFormatter) {
    row.add(localDateFormatter.printAsLocalTime(resource.getCreatedAt(), Constants.DATETIME_FORMAT));
    row.add(resource.getCreatedBy());
    row.add(localDateFormatter.printAsLocalTime(resource.getUpdatedAt(), Constants.DATETIME_FORMAT));
    row.add(resource.getUpdatedBy());
  }

}
