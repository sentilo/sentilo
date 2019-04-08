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
package org.sentilo.web.catalog.search.builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.domain.Permission.Type;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.util.StringUtils;

public class DefaultSearchFilterBuilderImpl implements SearchFilterBuilder {

  public static final String ID_COLUMN = "_id";
  public static final String NAME_COLUMN = "name";
  public static final String DESC_COLUMN = "description";
  public static final String CREATED_COLUMN = "createdAt";
  public static final String CREATED_BY_COLUMN = "createdBy";
  public static final String TYPE_COLUMN = "type";
  public static final String ENTITY_ID_COLUMN = "entityId";
  public static final String ENTITY_TYPE_COLUMN = "entityType";
  public static final String SENSORID_COLUMN = "sensorId";
  public static final String PROVIDERID_COLUMN = "providerId";
  public static final String MOBILE_COLUMN = "mobile";
  public static final String USER_NAME_COLUMN = "userName";
  public static final String EMAIL_COLUMN = "email";
  public static final String TARGET_COLUMN = "target";
  public static final String ENDPOINT_COLUMN = "endpoint";
  public static final String MAX_RETRIES_COLUMN = "maxRetries";
  public static final String RETRY_DELAY_COLUMN = "retryDelay";
  public static final String PROVIDER_COLUMN = "provider";
  public static final String PROVIDER_ID_COLUMN = "providerId";
  public static final String SENSOR_COLUMN = "sensor";
  public static final String ALARM_COLUMN = "alarm";
  public static final String COMPONENT_TYPE_COLUMN = "componentType";
  public static final String ACCESS_COLUMN = "publicAccess";
  public static final String STATE_COLUMN = "state";
  public static final String SUBSTATE_COLUMN = "substate";
  public static final String ORGANIZATION_COLUMN = "organization";
  public static final String ENTITY_COLUMN = "entity";
  public static final String DATE_COLUMN = "date";
  public static final String SENSOR_TYPE_COLUMN = "sensorType";
  public static final String CONTENT_TYPE_COLUMN = "contentType";
  public static final String ACTIVE_COLUMN = "active";
  public static final String TRIGGER_COLUMN = "trigger";
  public static final String SUBSCRIPTION_TYPE_COLUMN = "subscriptionType";
  public static final String APP_CLIENT_COLUMN = "appClientName";
  public static final String LAST_SYNC_COLUMN = "lastSyncTime";

  static final Map<String, Object> ACCESS_DICTIONARY = new HashMap<String, Object>();
  static final Map<String, Object> ACTIVE_DICTIONARY = new HashMap<String, Object>();
  static final Map<String, Object> PERMISSION_TYPE_DICTIONARY = new HashMap<String, Object>();

  static {
    ACCESS_DICTIONARY.put("true", true);
    ACCESS_DICTIONARY.put("false", false);
    ACCESS_DICTIONARY.put("Públic", true);
    ACCESS_DICTIONARY.put("Privat", false);
    ACCESS_DICTIONARY.put("Public", true);
    ACCESS_DICTIONARY.put("Private", false);
    ACCESS_DICTIONARY.put("Público", true);
    ACCESS_DICTIONARY.put("Privado", false);

    ACTIVE_DICTIONARY.put("true", true);
    ACTIVE_DICTIONARY.put("false", false);

    PERMISSION_TYPE_DICTIONARY.put("Lectura", Type.READ);
    PERMISSION_TYPE_DICTIONARY.put("Read", Type.READ);
    PERMISSION_TYPE_DICTIONARY.put("Lectura-Escritura", Type.WRITE);
    PERMISSION_TYPE_DICTIONARY.put("Lectura-Escriptura", Type.WRITE);
    PERMISSION_TYPE_DICTIONARY.put("Read-Write", Type.WRITE);
    PERMISSION_TYPE_DICTIONARY.put("Administració", Type.ADMIN);
    PERMISSION_TYPE_DICTIONARY.put("Administración", Type.ADMIN);
    PERMISSION_TYPE_DICTIONARY.put("Administration", Type.ADMIN);
  }

  public DefaultSearchFilterBuilderImpl() {
    registerDataTableColumns();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.search.builder.SearchFilterBuilder#buildMapSearchFilter()
   */
  @Override
  public SearchFilter buildMapSearchFilter() {
    final SearchFilter filter = new SearchFilter();

    final Authentication authetication = SecurityContextHolder.getContext().getAuthentication();
    final boolean userIsLoggedIn = authetication != null && authetication.getPrincipal() instanceof CatalogUserDetails;

    boolean requestToOwnTenant = true;

    if (TenantContextHolder.hasContext()) {
      // Views always shown components belonging to or authorized to request's tenant
      // And if request's tenant != user's tenant then only publics components are returned
      final String requestTenant = TenantUtils.getRequestTenant();
      final String userTenant = TenantUtils.getUserTenant();
      if (StringUtils.hasText(requestTenant)) {
        // Show only granted components that had been marked as visible on map
        filter.addAndParam("tenantsMapVisible", requestTenant);

        // Show only own and granted components
        filter.addAndParam("tenantsAuth", requestTenant);
      }

      requestToOwnTenant = SentiloUtils.areEquals(requestTenant, userTenant);
    }

    if (!userIsLoggedIn || !requestToOwnTenant) {
      filter.addAndParam("publicAccess", Boolean.TRUE);
    }

    return filter;
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.search.builder.SearchFilterBuilder#buildSearchFilter(javax.servlet.
   * http.HttpServletRequest, org.springframework.data.domain.Pageable, java.lang.String,
   * org.sentilo.web.catalog.security.service.CatalogUserDetailsService)
   */
  @Override
  public SearchFilter buildSearchFilter(final HttpServletRequest request, final Pageable pageable, final String wordToSearch,
      final CatalogUserDetailsService userDetailsService) {

    final boolean mustBeFilteredByTenant =
        TenantContextHolder.isEnabled() && dataMustBeFilteredByTenant(request, userDetailsService.getCatalogUserDetails());

    final Map<String, Object> params =
        StringUtils.hasText(wordToSearch) ? buildSearchParams(request, wordToSearch) : Collections.<String, Object>emptyMap();

    final SearchFilter searchFilter = new SearchFilter(params, pageable);

    if (mustBeFilteredByTenant) {
      final String[] tenantFilter = {TenantUtils.getRequestTenant()};
      // The field "tenantsListVisible" is a subgroup from "tenantsAuth" that filters those
      // resources that are not allowed to be showed on lists
      // searchFilter.addAndParam("tenantsAuth", tenantFilter);
      searchFilter.addAndParam("tenantsListVisible", tenantFilter);
    }

    return searchFilter;
  }

  protected Map<String, Object> buildSearchParams(final HttpServletRequest request, final String wordToSearch) {
    final Map<String, Object> searchParams = new HashMap<String, Object>();
    final List<Column> listColumns = SearchFilterUtils.getListColumns(request);

    for (final Column column : listColumns) {
      if (column.isSearchable()) {
        if (!column.isDictionaryEmpty()) {
          searchParams.put(column.getName(), column.dictionaryContainsWord(wordToSearch));
        } else {
          searchParams.put(column.getName(), wordToSearch);
        }
      }
    }

    return searchParams;
  }

  private boolean dataMustBeFilteredByTenant(final HttpServletRequest request, final CatalogUserDetails userDetails) {
    // Depending on the dataTable requested and the user role, data should be filtered by tenant
    final String[] noFilterDataTableNames =
        {"permissionTable", "componentTypeTable", "sensorTypeTable", "tenantTable", "tenantFromPermissionsTable", "tenantToPermissionsTable"};
    final List<String> noFilterDTNList = Arrays.asList(noFilterDataTableNames);

    final String tableName = request.getParameter("tableName");

    return noFilterDTNList.contains(tableName) || "userTable".equals(tableName) && userDetails.isSuperAdminUser() ? false : true;
  }

  private void registerDataTableColumns() {
    registerAlertDataTableColumns();
    registerAlertRuleDataTableColumns();
    registerProviderDataTableColumns();
    registerApplicationDataTableColumns();
    registerSensorDataTableColumns();
    registerComponentDataTableColumns();
    registerUserDataTableColumns();
    registerPermissionsDataTableColumns();
    registerSensorTypesDataTableColumns();
    registerComponentTypesDataTableColumns();
    registerSubscriptionsDataTableColumns();
    registerActiveSubscriptionsDataTableColumns();
    registerTenantDataTableColumns();
    registerTenantGrantsDataTableColumns();
    registerProviderDocumentFilesDataTableColumns();
    registerFederationConfigDataTableColumns();
  }

  private void registerAlertDataTableColumns() {
    final Map<String, Object> dictionary = new HashMap<String, Object>();
    dictionary.put("External", "EXTERNAL");
    dictionary.put("Internal", "INTERNAL");

    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(TYPE_COLUMN, true, true, dictionary));
    columns.add(new Column(TRIGGER_COLUMN, true, true));
    columns.add(new Column(ACTIVE_COLUMN, true, true, ACTIVE_DICTIONARY));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("alert", columns);
  }

  private void registerAlertRuleDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(PROVIDER_ID_COLUMN, true, true));
    columns.add(new Column(COMPONENT_TYPE_COLUMN, true, true));
    columns.add(new Column(SENSOR_TYPE_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("alertRule", columns);
  }

  private void registerProviderDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("provider", columns);
  }

  private void registerApplicationDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("application", columns);
  }

  private void registerSensorDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(SENSORID_COLUMN, true, true));
    columns.add(new Column(PROVIDERID_COLUMN, true, true));
    columns.add(new Column(TYPE_COLUMN, true, true));
    columns.add(new Column(ACCESS_COLUMN, true, true, ACCESS_DICTIONARY));
    columns.add(new Column(STATE_COLUMN, true, true));
    columns.add(new Column(SUBSTATE_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("sensor", columns);
  }

  private void registerComponentDataTableColumns() {
    final Map<String, Object> dictionary = new HashMap<String, Object>();
    dictionary.put("Static", 0);// English
    dictionary.put("Mobile", 1);
    dictionary.put("Estàtic", 0);// Catalan
    dictionary.put("Mòbil", 1);

    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(PROVIDERID_COLUMN, true, true));
    columns.add(new Column(MOBILE_COLUMN, true, true, dictionary));
    columns.add(new Column(COMPONENT_TYPE_COLUMN, true, true));
    columns.add(new Column(ACCESS_COLUMN, true, true, ACCESS_DICTIONARY));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("component", columns);
  }

  private void registerUserDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(EMAIL_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("users", columns);
  }

  private void registerPermissionsDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(TARGET_COLUMN, true, true));
    columns.add(new Column(TYPE_COLUMN, true, true, PERMISSION_TYPE_DICTIONARY));

    SearchFilterUtils.addListColumns("permissions", columns);
  }

  private void registerSensorTypesDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("sensortypes", columns);
  }

  private void registerComponentTypesDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("componenttypes", columns);
  }

  private void registerSubscriptionsDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(SUBSCRIPTION_TYPE_COLUMN));
    columns.add(new Column(PROVIDER_COLUMN));
    columns.add(new Column(SENSOR_COLUMN));
    columns.add(new Column(ALARM_COLUMN));
    columns.add(new Column(ENDPOINT_COLUMN));
    columns.add(new Column(MAX_RETRIES_COLUMN));
    columns.add(new Column(RETRY_DELAY_COLUMN));

    SearchFilterUtils.addListColumns("subscriptions", columns);
  }

  private void registerActiveSubscriptionsDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();

    columns.add(new Column(ENTITY_ID_COLUMN, true, true));
    columns.add(new Column(ENTITY_TYPE_COLUMN, true, true));
    columns.add(new Column(SUBSCRIPTION_TYPE_COLUMN, true, true));
    columns.add(new Column(PROVIDER_COLUMN, true, true));
    columns.add(new Column(SENSOR_COLUMN, true, true));
    columns.add(new Column(ENDPOINT_COLUMN, true, true));
    columns.add(new Column(MAX_RETRIES_COLUMN, true, true));
    columns.add(new Column(RETRY_DELAY_COLUMN, true, true));

    SearchFilterUtils.addListColumns("activesubscriptions", columns);
  }

  private void registerTenantDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("tenant", columns);
  }

  private void registerTenantGrantsDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ORGANIZATION_COLUMN));
    columns.add(new Column(ENTITY_COLUMN));
    columns.add(new Column(TYPE_COLUMN));
    columns.add(new Column(DATE_COLUMN));
    columns.add(new Column(USER_NAME_COLUMN));

    SearchFilterUtils.addListColumns("grants", columns);
  }

  private void registerProviderDocumentFilesDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(DESC_COLUMN, true, true));
    columns.add(new Column(CONTENT_TYPE_COLUMN, true, true));
    columns.add(new Column(CREATED_BY_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("documents", columns);
  }

  private void registerFederationConfigDataTableColumns() {
    final List<Column> columns = new ArrayList<Column>();
    columns.add(new Column(ID_COLUMN, true, true));
    columns.add(new Column(NAME_COLUMN, true, true));
    columns.add(new Column(APP_CLIENT_COLUMN, true, true));
    columns.add(new Column(LAST_SYNC_COLUMN, true, true));
    columns.add(new Column(CREATED_COLUMN, true, true));

    SearchFilterUtils.addListColumns("federation", columns);
  }

}
