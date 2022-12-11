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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.context.DataTablesContextHolder;
import org.sentilo.web.catalog.context.DataTablesContextImpl;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.SectorGrant;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;
import org.sentilo.web.catalog.domain.SectorResourceGranted;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.domain.Tenant;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.service.impl.AbstractBaseCrudServiceImpl;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.SectorUtils;
import org.springframework.context.MessageSource;
import org.springframework.data.util.Pair;

public class SectorUtilsTest {

  private static final String SECTOR_ID = "mockSector1";

  @Mock
  private HttpServletRequest request;

  @Mock
  private MessageSource messageSource;

  @Mock
  private AbstractBaseCrudServiceImpl<Provider> service;

  @Mock
  private CatalogUserDetails userDetails;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(request.getParameter(Constants.SECTOR_ID_REQUEST_PARAM)).thenReturn(SECTOR_ID);
  }

  @After
  public void tearDown() {
    DataTablesContextHolder.clearContext();
  }

  @Test
  public void addAndFilterBySectorToResourceSearch() {
    final List<Pair<Class<?>, String>> resourcesAndParams =
        Arrays.asList(Pair.of(User.class, "sectors"), Pair.of(Provider.class, "sectors.sectorId"), Pair.of(Application.class, "sectors.sectorId"));

    resourcesAndParams.forEach(resource ->
      {
        final String resourceName = resource.getFirst().getSimpleName().toLowerCase();
        final String tableName = resourceName + "InSectorTable";
        final SearchFilter filter = new SearchFilter();
        DataTablesContextHolder.setContext(new DataTablesContextImpl(request));
        when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableName);

        SectorUtils.addFilterBySectorIfNeedBe(filter, resource.getFirst());

        Assert.assertTrue(SECTOR_ID.equals(filter.getAndParams().get(resource.getSecond())));
        DataTablesContextHolder.clearContext();
      });
  }

  @Test
  public void addNorFilterBySectorToResourceSearch() {
    final List<Pair<Class<?>, String>> resourcesAndParams =
        Arrays.asList(Pair.of(User.class, "sectors"), Pair.of(Provider.class, "sectors.sectorId"), Pair.of(Application.class, "sectors.sectorId"));

    resourcesAndParams.forEach(resource ->
      {
        final String resourceName = resource.getFirst().getSimpleName().toLowerCase();
        final String tableName = resourceName + "NotInSectorTable";
        final SearchFilter filter = new SearchFilter();
        DataTablesContextHolder.setContext(new DataTablesContextImpl(request));
        when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableName);

        SectorUtils.addFilterBySectorIfNeedBe(filter, resource.getFirst());

        Assert.assertTrue(SECTOR_ID.equals(filter.getNorParams().get(resource.getSecond())));
        DataTablesContextHolder.clearContext();
      });
  }

  @SuppressWarnings("serial")
  @Test
  public void addGrantDescriptionColumnToRow() {
    final String grantDesc = "mock_grant";
    final List<SectorResourceGranted> resources = Arrays.asList(new Provider("mockProv1"), new Application("mockApp1"));
    final Set<SectorGrant> sectors = new HashSet<SectorGrant>() {

      /**
       *
       */
      private static final long serialVersionUID = 1L;

      {
        add(new SectorGrant(SECTOR_ID, GrantType.A));
        add(new SectorGrant("mockSector2", GrantType.A));
      }
    };

    when(messageSource.getMessage(any(), any(), any())).thenReturn(grantDesc);

    resources.forEach(resource ->
      {
        resource.getSectors().addAll(sectors);
        final List<String> row = new ArrayList<String>();
        final String resourceName = resource.getClass().getSimpleName().toLowerCase();
        final String tableName = resourceName + "InSectorTable";
        DataTablesContextHolder.setContext(new DataTablesContextImpl(request));
        when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableName);

        SectorUtils.addGrantDescriptionColumnToRowIfNeedBy(row, resource, messageSource);

        Assert.assertTrue(row.get(0).equals(grantDesc));
      });
  }

  @Test
  public void addEmptyGrantDescriptionColumnToRow() {
    final List<SectorResourceGranted> resources = Arrays.asList(new Provider("mockProv1"), new Application("mockApp1"));

    resources.forEach(resource ->
      {
        final List<String> row = new ArrayList<String>();
        final String resourceName = resource.getClass().getSimpleName().toLowerCase();
        final String tableName = resourceName + "NotInSectorTable";
        DataTablesContextHolder.setContext(new DataTablesContextImpl(request));
        when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableName);

        SectorUtils.addGrantDescriptionColumnToRowIfNeedBy(row, resource, messageSource);

        Assert.assertTrue(row.get(0).isEmpty());
      });
  }

  @SuppressWarnings("unchecked")
  @Test
  public void buildGrantedSectorFilterTest() {
    final List<String> sectorsIds = Arrays.asList("sector1", "sector2");
    final SearchFilter sf = SectorUtils.buildGrantedSectorFilter(sectorsIds);
    Assert.assertTrue(sf.getAndParams().get("sectors") != null && ((List<SectorGrant>) sf.getAndParams().get("sectors")).size() == sectorsIds.size());
  }

  @Test
  public void existSectorWithAdminGrant() {
    final List<String> sectors = Arrays.asList("mock_sector1", "mock_sector2", "mock_sector3");
    final List<SectorGrant> sectorGrants_1 =
        Arrays.asList(new SectorGrant("mock_sector1", GrantType.A), new SectorGrant("mock_sector2", GrantType.R));
    final List<SectorGrant> sectorGrants_2 = Arrays.asList(new SectorGrant("mock_sector3", GrantType.R));

    Assert.assertTrue(SectorUtils.existSectorWithAdminGrant(sectors, sectorGrants_1));
    Assert.assertFalse(SectorUtils.existSectorWithAdminGrant(sectors, sectorGrants_2));
  }

  @Test
  public void isRelatedToSectorList() {
    final Application app = new Application();
    final String tableNameNotIn = app.getClass().getSimpleName().toLowerCase() + "NotInSectorTable";
    final String tableNameIn = app.getClass().getSimpleName().toLowerCase() + "InSectorTable";
    DataTablesContextHolder.setContext(new DataTablesContextImpl(request));
    when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableNameNotIn, tableNameIn, tableNameIn);

    Assert.assertTrue(SectorUtils.isAddToSectorList(app));
    Assert.assertTrue(SectorUtils.isSectorResourceList(app));
    Assert.assertTrue(SectorUtils.isSectorList(app));
  }

  @Test
  public void canSectorialUserAdminResource() {
    final List<String> userSectors = Arrays.asList("mock_sector1", "mock_sector2", "mock_sector3");
    final List<SectorGrant> resourceSectorGrants_1 =
        Arrays.asList(new SectorGrant("mock_sector1", GrantType.A), new SectorGrant("mock_sector2", GrantType.R));
    final List<SectorGrant> resourceSectorGrants_2 = Arrays.asList(new SectorGrant("mock_sector3", GrantType.R));
    final Provider resourceWithAdminGrant = mock(Provider.class);
    final Provider resourceWithReadGrant = mock(Provider.class);
    final Provider resourceNoGranted = mock(Provider.class);
    final User userInSectors = mock(User.class);
    final User userNoInSectors = mock(User.class);

    when(userDetails.isSectorialUser()).thenReturn(true);
    when(userDetails.getSectors()).thenReturn(userSectors);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(resourceWithAdminGrant.getSectors()).thenReturn(resourceSectorGrants_1);
    when(resourceWithReadGrant.getSectors()).thenReturn(resourceSectorGrants_2);
    when(resourceNoGranted.getSectors()).thenReturn(Collections.emptyList());
    when(userInSectors.getSectors()).thenReturn(Arrays.asList("mock_sector2"));
    when(userNoInSectors.getSectors()).thenReturn(Collections.emptyList());

    Assert.assertTrue(SectorUtils.canSectorialUserAdminResource(userDetails, resourceWithAdminGrant));
    Assert.assertFalse(SectorUtils.canSectorialUserAdminResource(userDetails, resourceWithReadGrant));
    Assert.assertFalse(SectorUtils.canSectorialUserAdminResource(userDetails, resourceNoGranted));
    Assert.assertTrue(SectorUtils.canSectorialUserAdminResource(userDetails, userInSectors));
    Assert.assertFalse(SectorUtils.canSectorialUserAdminResource(userDetails, userNoInSectors));
  }

  @Test
  public void canSectorialUserReadResource() {
    final List<String> userSectors = Arrays.asList("mock_sector1", "mock_sector2", "mock_sector3");
    final List<SectorGrant> resourceSectorGrants_1 =
        Arrays.asList(new SectorGrant("mock_sector1", GrantType.A), new SectorGrant("mock_sector2", GrantType.R));
    final List<SectorGrant> resourceSectorGrants_2 = Arrays.asList(new SectorGrant("mock_sector3", GrantType.R));
    final Provider resourceWithAdminGrant = mock(Provider.class);
    final Provider resourceWithReadGrant = mock(Provider.class);
    final Provider resourceNoGranted = mock(Provider.class);
    final User userInSectors = mock(User.class);
    final User userNoInSectors = mock(User.class);
    final Tenant noSectorialResourceNoAllowed = mock(Tenant.class);
    final ComponentType noSectorialResourceAllowed_1 = mock(ComponentType.class);
    final SensorType noSectorialResourceAllowed_2 = mock(SensorType.class);
    final Sector sectorAllowed = mock(Sector.class);
    final Sector sectorNoAllowed = mock(Sector.class);

    when(userDetails.isSectorialUser()).thenReturn(true);
    when(userDetails.getSectors()).thenReturn(userSectors);
    when(userDetails.isAdminUser()).thenReturn(true);
    when(resourceWithAdminGrant.getSectors()).thenReturn(resourceSectorGrants_1);
    when(resourceWithReadGrant.getSectors()).thenReturn(resourceSectorGrants_2);
    when(resourceNoGranted.getSectors()).thenReturn(Collections.emptyList());
    when(userInSectors.getSectors()).thenReturn(Arrays.asList("mock_sector2"));
    when(userNoInSectors.getSectors()).thenReturn(Collections.emptyList());
    when(sectorAllowed.getId()).thenReturn("mock_sector1");
    when(sectorNoAllowed.getId()).thenReturn("mock_sector5");

    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, resourceWithAdminGrant));
    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, resourceWithReadGrant));
    Assert.assertFalse(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, resourceNoGranted));
    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, userInSectors));
    Assert.assertFalse(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, userNoInSectors));
    Assert.assertFalse(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, noSectorialResourceNoAllowed));
    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, noSectorialResourceAllowed_1));
    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, noSectorialResourceAllowed_2));
    Assert.assertTrue(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, sectorAllowed));
    Assert.assertFalse(SectorUtils.canSectorialUserAtLeastReadResource(userDetails, sectorNoAllowed));
  }

}
