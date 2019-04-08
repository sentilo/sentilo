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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.enums.AlertTriggerType;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Alert.Type;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

public abstract class ApiAlertConverter {

  private static final String NOT_VALID_ALERT_TYPE_MESSAGE =
      "Alert %s: invalid alert type value %s. Review doc to see which are valid values for that field.";
  private static final String NOT_VALID_ALERT_TRIGGER_TYPE_MESSAGE =
      "Alert %s: invalid trigger type value %s. Review doc to see which are valid values for that field.";
  private static final String ALERT_NOT_FOUND = "Alert %s: alert with this id and type %s has not been found in the system.";

  private ApiAlertConverter() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static List<CatalogAlert> convertToCatalogAlertList(final List<Alert> alerts) {
    final List<CatalogAlert> catalogAlerts = new ArrayList<CatalogAlert>();

    for (final Alert alert : alerts) {
      final String entityId = StringUtils.hasText(alert.getProviderId()) ? alert.getProviderId() : alert.getApplicationId();
      final CatalogAlert catalogAlert = new CatalogAlert();
      catalogAlert.setId(alert.getId());
      catalogAlert.setName(alert.getName());
      catalogAlert.setDescription(alert.getDescription());
      catalogAlert.setEntity(entityId);
      catalogAlert.setType(alert.getType().name());
      catalogAlert.setActive(Boolean.toString(alert.isActive()));
      catalogAlert.setCreatedAt(alert.getCreatedAt().getTime());
      catalogAlert.setUpdatedAt(alert.getUpdatedAt().getTime());

      if (Type.INTERNAL.equals(alert.getType())) {
        catalogAlert.setTrigger(alert.getTrigger().name());
        catalogAlert.setExpression(alert.getExpression());
        catalogAlert.setComponent(CompoundKeyBuilder.splitCompoundKey(alert.getComponentId())[1]);
        catalogAlert.setSensor(alert.getSensorId());
      }

      catalogAlerts.add(catalogAlert);
    }

    return catalogAlerts;
  }

  public static List<Alert> buildAlertsFromCatalogAlerts(final ApiAlertConverterContext context, final String catalogUser) {
    final List<Alert> alerts = new ArrayList<Alert>();
    final List<CatalogAlert> catalogAlerts = context.getMessage().getAlerts();
    final boolean isUpdateAction = context.isUpdateAction();

    validateCatalogAlertEnumData(context);

    if (!context.getResults().hasErrors() && !CollectionUtils.isEmpty(catalogAlerts)) {
      for (final CatalogAlert catalogAlert : catalogAlerts) {
        final Alert alert =
            isUpdateAction ? buildAlertToUpdate(catalogAlert, context, catalogUser) : buildNewAlert(catalogAlert, context, catalogUser);
        alerts.add(alert);
      }
    }

    return alerts;
  }

  private static Alert buildNewAlert(final CatalogAlert catalogAlert, final ApiAlertConverterContext context, final String catalogUser) {
    final Alert alert = new Alert();
    final String entityId = StringUtils.hasText(catalogAlert.getEntity()) ? catalogAlert.getEntity() : context.getMessage().getEntityId();

    setCommonAttributes(alert, catalogAlert, catalogUser);

    if (catalogAlert.getType() != null) {
      switch (Type.valueOf(catalogAlert.getType())) {
        case EXTERNAL:
          setExternalAttributes(alert, entityId, context);
          break;
        case INTERNAL:
          setInternalAttributes(alert, entityId, catalogAlert);
          break;
        default:
          break;
      }
    }

    return alert;
  }

  private static void setCommonAttributes(final Alert alert, final CatalogAlert catalogAlert, final String catalogUser) {
    alert.setId(catalogAlert.getId());
    alert.setName(catalogAlert.getName());
    alert.setDescription(catalogAlert.getDescription());
    alert.setCreatedAt(new Date());
    alert.setCreatedBy(catalogUser);
    alert.setActive(true);
  }

  private static void setExternalAttributes(final Alert alert, final String entityId, final ApiAlertConverterContext context) {
    final boolean isEntityProvider = context.getProviderService().exists(entityId);
    alert.setType(Type.EXTERNAL);
    if (isEntityProvider) {
      alert.setProviderId(entityId);
    } else {
      alert.setApplicationId(entityId);
    }
  }

  private static void setInternalAttributes(final Alert alert, final String entityId, final CatalogAlert catalogAlert) {
    alert.setType(Type.INTERNAL);
    alert.setProviderId(entityId);
    alert.setComponentId(CompoundKeyBuilder.buildCompoundKey(entityId, catalogAlert.getComponent()));
    alert.setSensorId(catalogAlert.getSensor());
    alert.setTrigger(AlertTriggerType.valueOf(catalogAlert.getTrigger()));
    alert.setExpression(catalogAlert.getExpression());
  }

  private static Alert buildAlertToUpdate(final CatalogAlert catalogAlert, final ApiAlertConverterContext context, final String catalogUser) {
    final Alert alert = context.getAlertService().find(new Alert(catalogAlert.getId()));
    final String entityId = StringUtils.hasText(catalogAlert.getEntity()) ? catalogAlert.getEntity() : context.getMessage().getEntityId();

    // To confirm that an alert exists not only alert must be not null but also type and owner must
    // be equals
    if (alert != null && verifyType(alert.getType(), catalogAlert.getType()) && verifyOwner(alert, entityId)) {
      if (SentiloUtils.stringIsNotEmptyOrNull(catalogAlert.getName())) {
        alert.setName(catalogAlert.getName());
      }

      if (SentiloUtils.stringIsNotEmptyOrNull(catalogAlert.getDescription())) {
        alert.setDescription(catalogAlert.getDescription());
      }

      if (Type.INTERNAL.equals(alert.getType())) {
        if (SentiloUtils.stringIsNotEmptyOrNull(catalogAlert.getTrigger())) {
          alert.setTrigger(AlertTriggerType.valueOf(catalogAlert.getTrigger()));
        }

        if (SentiloUtils.stringIsNotEmptyOrNull(catalogAlert.getExpression())) {
          alert.setExpression(catalogAlert.getExpression());
        }
      }

      alert.setUpdatedAt(new Date());
      alert.setUpdatedBy(catalogUser);
    } else {
      final String errorMessage = String.format(ALERT_NOT_FOUND, catalogAlert.getId(), catalogAlert.getType());
      context.getResults().addErrorMessage(errorMessage);
    }

    return alert;
  }

  private static boolean verifyType(final Type expected, final String actual) {
    return expected != null && expected.name().equals(actual);
  }

  private static boolean verifyOwner(final Alert alert, final String actual) {
    final String owner = StringUtils.hasText(alert.getApplicationId()) ? alert.getApplicationId() : alert.getProviderId();
    return owner.equals(actual);
  }

  private static void validateCatalogAlertEnumData(final ApiAlertConverterContext context) {
    final List<CatalogAlert> catalogAlerts = context.getMessage().getAlerts();
    if (!CollectionUtils.isEmpty(context.getMessage().getAlerts())) {
      for (final CatalogAlert catalogAlert : catalogAlerts) {
        if (!validAlertType(catalogAlert.getType())) {
          final String errorMessage = String.format(NOT_VALID_ALERT_TYPE_MESSAGE, catalogAlert.getName(), catalogAlert.getType());
          context.getResults().addErrorMessage(errorMessage);
        }
        if (!validTriggerType(catalogAlert.getTrigger())) {
          final String errorMessage = String.format(NOT_VALID_ALERT_TRIGGER_TYPE_MESSAGE, catalogAlert.getName(), catalogAlert.getTrigger());
          context.getResults().addErrorMessage(errorMessage);
        }
      }
    }
  }

  private static boolean validAlertType(final String value) {
    boolean result = true;
    if (StringUtils.hasText(value)) {
      try {
        Type.valueOf(value);
      } catch (final IllegalArgumentException ecnpe) {
        result = false;
      }
    }

    return result;
  }

  private static boolean validTriggerType(final String value) {
    boolean result = true;
    if (StringUtils.hasText(value)) {
      try {
        AlertTriggerType.valueOf(value);
      } catch (final IllegalArgumentException ecnpe) {
        result = false;
      }
    }

    return result;
  }

}
