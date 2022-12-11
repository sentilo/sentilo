/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.sentilo.web.catalog.context.DataTablesContextHolder;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.SectorGrant;
import org.sentilo.web.catalog.domain.SectorResource;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;
import org.sentilo.web.catalog.domain.SectorResourceGranted;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.util.CollectionUtils;

public abstract class SectorUtils extends CatalogUtils {

  private static final String IN_SECTOR_TABLE_NAME_SUFFIX = "InSectorTable";
  private static final String NOT_IN_SECTOR_TABLE_NAME_SUFFIX = "NotInSectorTable";

  private static Map<String, String> sectorFilterNames = new HashMap<String, String>();
  static {
    sectorFilterNames.put("user", "sectors");
    sectorFilterNames.put("provider", "sectors.sectorId");
    sectorFilterNames.put("application", "sectors.sectorId");
  }

  private SectorUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  /**
   * Adds a new filter criteria by sector depending on tableName value: if tableName equals to
   * resources_in_sector_table then adds an AND param, else if tableName equals to
   * resources_not_in_sector_table then adds an NOR param. Otherwise nothing is added
   *
   * @param filter
   * @param resource Class of the sector's resource to search
   */
  public static <T> void addFilterBySectorIfNeedBe(final SearchFilter filter, final Class<T> resource) {
    final String resourceName = resource.getSimpleName().toLowerCase();
    final String sectorFilterName = sectorFilterNames.get(resourceName);
    final String resourceInSectorTableName = resourceName + IN_SECTOR_TABLE_NAME_SUFFIX;
    final String resourceNotInSectorTableName = resourceName + NOT_IN_SECTOR_TABLE_NAME_SUFFIX;

    // Sector filter
    final String sectorId = DataTablesContextHolder.getContext().getParam(Constants.SECTOR_ID_REQUEST_PARAM);
    final String tableName = DataTablesContextHolder.getContext().getTableName();
    if (tableName.equals(resourceInSectorTableName)) {
      filter.addAndParam(sectorFilterName, sectorId);
    } else if (tableName.equals(resourceNotInSectorTableName)) {
      filter.addNorParam(sectorFilterName, sectorId);
    }
  }

  /**
   * Add one additional column to row, with the description of sector's grant, when tableName refers
   * to either resources in sector or resources not in sector
   *
   * @param row
   * @param resource
   * @param messageSource
   */
  public static void addGrantDescriptionColumnToRowIfNeedBy(final List<String> row, final SectorResourceGranted resource,
      final MessageSource messageSource) {
    final String tableName = DataTablesContextHolder.getContext().getTableName();
    final String resourceName = resource.getClass().getSimpleName().toLowerCase();
    final String resourceInSectorTableName = resourceName + IN_SECTOR_TABLE_NAME_SUFFIX;
    final String resourceNotInSectorTableName = resourceName + NOT_IN_SECTOR_TABLE_NAME_SUFFIX;

    if (tableName.equals(resourceInSectorTableName) || tableName.equals(resourceNotInSectorTableName)) {
      final String sectorId = DataTablesContextHolder.getContext().getParam(Constants.SECTOR_ID_REQUEST_PARAM);
      final Optional<GrantType> sectorGrantInResource =
          resource.getSectors().stream().filter(sg -> sg.getSectorId().equals(sectorId)).map(SectorGrant::getGrant).findFirst();

      if (sectorGrantInResource.isPresent()) {
        row.add(messageSource.getMessage(Constants.SECTOR_GRANT_MESSAGE + sectorGrantInResource.get().name(), null, LocaleContextHolder.getLocale()));
      } else {
        row.add("");
      }

    }
  }

  /**
   * Generate SearchFilter with all sectors with GrantType.Admin to be searched and filtered.
   * 
   * @param sectorsIds - The ids to add in filter
   * @return a SearchFilter with all sectors received with GranType.Admin
   */
  public static SearchFilter buildGrantedSectorFilter(final List<String> sectorsIds) {
    final SearchFilter sf = new SearchFilter();
    if (!CollectionUtils.isEmpty(sectorsIds)) {
      sf.addAndParam("sectors", sectorsIds.stream().map(id -> new SectorGrant(id, GrantType.A)).collect(Collectors.toList()));
    }
    return sf;
  }

  /**
   * Return true if exists one sector from sectors set with A grant in grants (utility to check when
   * a sectorial user can manage a resource).
   *
   * @param sectors
   * @param grants
   * @return
   */
  public static boolean existSectorWithAdminGrant(final List<String> sectors, final List<SectorGrant> grants) {
    return !CollectionUtils.isEmpty(grants)
        && !grants.stream().filter(sectorGrant -> sectorGrant.getGrant().equals(GrantType.A) && sectors.contains(sectorGrant.getSectorId()))
            .collect(Collectors.toList()).isEmpty();
  }

  /**
   * Check if datatable to be display is one related to adding resources to a sector
   *
   * @param resource
   * @return true if tableName is equals, false otherwise
   */
  public static boolean isAddToSectorList(final SectorResource<?> resource) {
    return isRelatedToSectorList(resource, NOT_IN_SECTOR_TABLE_NAME_SUFFIX);
  }

  /**
   * Check if datatable to be display is one related to showing resources from a sector
   *
   * @param resource
   * @return
   */
  public static boolean isSectorResourceList(final SectorResource<?> resource) {
    return isRelatedToSectorList(resource, IN_SECTOR_TABLE_NAME_SUFFIX);
  }

  /**
   * Check if datatable to be display is one related to add or show resources from a sector
   *
   * @param resource
   * @return
   */
  public static boolean isSectorList(final SectorResource<?> resource) {
    return isRelatedToSectorList(resource, NOT_IN_SECTOR_TABLE_NAME_SUFFIX, IN_SECTOR_TABLE_NAME_SUFFIX);
  }

  /**
   * Return true if sectorial user can manage resource given, i.e., resource is a sectorial resource
   * and has granted an admin privilege for one of user's sectors.
   *
   * @param sectorialUser
   * @param resource
   * @return true if user can manage resource
   */
  @SuppressWarnings("unchecked")
  public static boolean canSectorialUserAdminResource(final CatalogUserDetails sectorialUser, final CatalogDocument resource) {
    boolean allowed = false;
    final boolean isSectorialResource = resource instanceof SectorResource<?>;
    final boolean isSectorialGrantResource = resource instanceof SectorResourceGranted;

    if (!isSectorialResource) {
      allowed = false;
    } else if (isSectorialGrantResource) {
      allowed = existSectorWithAdminGrant(sectorialUser.getSectors(), ((SectorResourceGranted) resource).getSectors());
    } else {
      allowed = !org.apache.commons.collections.CollectionUtils
          .intersection(sectorialUser.getSectors(), ((SectorResource<String>) resource).getSectors()).isEmpty();
    }

    return allowed;
  }

  /**
   * Return true if sectorial user can read resource given, i.e., resource must be one associated
   * with user sectors or resource must be a global resource which user can read (as componentType o
   * sensorType)
   *
   * @param sectorialUser
   * @param resource
   * @return true if user can read resource
   */
  @SuppressWarnings("unchecked")
  public static boolean canSectorialUserAtLeastReadResource(final CatalogUserDetails sectorialUser, final CatalogDocument resource) {
    boolean allowed = false;
    final boolean isSectorialResource = resource instanceof SectorResource<?>;
    final boolean isSectorialGrantResource = resource instanceof SectorResourceGranted;

    if (!isSectorialResource) {
      allowed = resource instanceof ComponentType || resource instanceof SensorType
          || (resource instanceof Sector && sectorialUser.getSectors().contains(resource.getId()));
    } else {
      final List<String> resourceSectors =
          isSectorialGrantResource ? getSectors((SectorResourceGranted) resource) : ((SectorResource<String>) resource).getSectors();

      allowed = !org.apache.commons.collections.CollectionUtils.intersection(sectorialUser.getSectors(), resourceSectors).isEmpty();
    }

    return allowed;
  }

  private static List<String> getSectors(final SectorResourceGranted resource) {
    return resource.getSectors().stream().map(sectorGrant -> sectorGrant.getSectorId()).collect(Collectors.toList());
  }

  /**
   * Check if dataTable name has a name that match any of the given
   *
   * @param resource
   * @param suffixesToCheck - List of suffixes to check
   *
   * @return true if tableName check any of the validations
   */
  private static boolean isRelatedToSectorList(final SectorResource<?> resource, final String... suffixesToCheck) {
    final String tableName = DataTablesContextHolder.getContext().getTableName();
    final String resourceName = resource.getClass().getSimpleName().toLowerCase();
    boolean result = false;

    for (final String suffix : suffixesToCheck) {
      final String tableNameToCheck = resourceName + suffix;
      result = result || tableName.equals(tableNameToCheck);
    }

    return result;
  }
}
