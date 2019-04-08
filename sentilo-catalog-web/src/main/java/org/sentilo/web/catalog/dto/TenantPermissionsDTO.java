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
package org.sentilo.web.catalog.dto;

import java.util.ArrayList;
import java.util.List;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.domain.TenantPermission;

public class TenantPermissionsDTO extends AbstractListDTO {

  private String parentEntityId;

  private List<OptionDTO> providers;
  private List<OptionDTO> entities;

  private TenantPermission.Type permissionType;

  private String[] selectedProvidersIds;
  private String[] selectedEntitiesIds;

  private Boolean visible;

  private Boolean listVisible;

  public TenantPermissionsDTO() {
    providers = new ArrayList<OptionDTO>();
    entities = new ArrayList<OptionDTO>();
    visible = false;
    listVisible = true;
  }

  public TenantPermissionsDTO(final String parentEntityId, final List<OptionDTO> providers, final List<OptionDTO> entities) {
    this();
    this.parentEntityId = parentEntityId;
    this.providers = providers;
    this.entities = entities;
  }

  public List<TenantPermission> getSelectedProviderPermissions() {
    return toPermissionList(selectedProvidersIds);
  }

  public List<TenantPermission> getSelectedTenantPermissions() {
    return toPermissionList(selectedEntitiesIds);
  }

  public List<TenantPermission> getSelectedPermissions() {
    return toPermissionList(getSelectedIds());
  }

  private List<TenantPermission> toPermissionList(final String[] selectedIds) {
    final List<TenantPermission> permissions = new ArrayList<TenantPermission>();
    if (!SentiloUtils.arrayIsEmpty(selectedIds)) {
      for (final String id : selectedIds) {
        final TenantPermission permission = new TenantPermission(id);
        permissions.add(permission);
      }
    }
    return permissions;
  }

  public String getParentEntityId() {
    return parentEntityId;
  }

  public void setParentEntityId(final String entityId) {
    parentEntityId = entityId;
  }

  public TenantPermission.Type getPermissionType() {
    return permissionType;
  }

  public void setPermissionType(final TenantPermission.Type permissionType) {
    this.permissionType = permissionType;
  }

  public List<OptionDTO> getProviders() {
    return providers;
  }

  public void setProviders(final List<OptionDTO> providers) {
    this.providers = providers;
  }

  public List<OptionDTO> getEntities() {
    return entities;
  }

  public void setEntities(final List<OptionDTO> entities) {
    this.entities = entities;
  }

  public String[] getSelectedProvidersIds() {
    return selectedProvidersIds;
  }

  public void setSelectedProvidersIds(final String[] selectedProvidersIds) {
    this.selectedProvidersIds = selectedProvidersIds;
  }

  public String[] getSelectedEntitiesIds() {
    return selectedEntitiesIds;
  }

  public void setSelectedEntitiesIds(final String[] selectedEntitiesIds) {
    this.selectedEntitiesIds = selectedEntitiesIds;
  }

  public Boolean getVisible() {
    return visible;
  }

  public void setVisible(final Boolean visible) {
    this.visible = visible;
  }

  public Boolean getListVisible() {
    return listVisible;
  }

  public void setListVisible(final Boolean listVisible) {
    this.listVisible = listVisible;
  }

}
