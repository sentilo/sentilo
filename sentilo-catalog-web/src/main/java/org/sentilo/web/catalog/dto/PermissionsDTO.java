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
import org.sentilo.web.catalog.domain.Permission;

public class PermissionsDTO extends AbstractListDTO {

  private String parentEntityId;

  private List<OptionDTO> providers;
  private List<OptionDTO> applications;

  private Permission.Type permissionType;

  private String[] selectedApplicationsIds;
  private String[] selectedProvidersIds;

  public PermissionsDTO() {
    providers = new ArrayList<OptionDTO>();
  }

  public PermissionsDTO(final String parentEntityId, final List<OptionDTO> providers, final List<OptionDTO> applications) {
    this.parentEntityId = parentEntityId;
    this.providers = providers;
    this.applications = applications;
  }

  public List<Permission> getSelectedProviderPermissions() {
    return toPermissionList(selectedProvidersIds);
  }

  public List<Permission> getSelectedApplicationPermissions() {
    return toPermissionList(selectedApplicationsIds);
  }

  public List<Permission> getSelectedPermissions() {
    return toPermissionList(getSelectedIds());
  }

  private List<Permission> toPermissionList(final String[] selectedIds) {
    final List<Permission> permissions = new ArrayList<Permission>();
    if (!SentiloUtils.arrayIsEmpty(selectedIds)) {
      for (final String id : selectedIds) {
        final Permission permission = new Permission();
        permission.setId(id);
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

  public Permission.Type getPermissionType() {
    return permissionType;
  }

  public void setPermissionType(final Permission.Type permissionType) {
    this.permissionType = permissionType;
  }

  public List<OptionDTO> getProviders() {
    return providers;
  }

  public void setProviders(final List<OptionDTO> providers) {
    this.providers = providers;
  }

  public List<OptionDTO> getApplications() {
    return applications;
  }

  public void setApplications(final List<OptionDTO> applications) {
    this.applications = applications;
  }

  public String[] getSelectedApplicationsIds() {
    return selectedApplicationsIds;
  }

  public void setSelectedApplicationsIds(final String[] selectedApplicationsIds) {
    this.selectedApplicationsIds = selectedApplicationsIds;
  }

  public String[] getSelectedProvidersIds() {
    return selectedProvidersIds;
  }

  public void setSelectedProvidersIds(final String[] selectedProvidersIds) {
    this.selectedProvidersIds = selectedProvidersIds;
  }
}
