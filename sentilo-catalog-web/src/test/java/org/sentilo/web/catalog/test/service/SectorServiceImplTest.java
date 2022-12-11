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
package org.sentilo.web.catalog.test.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Alert;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Provider;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.repository.SectorRepository;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.impl.SectorServiceImpl;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

public class SectorServiceImplTest extends AbstractBaseCrudServiceImplTest {

  private final static String SECTOR_ID = "sector1";

  @InjectMocks
  private SectorServiceImpl sectorService;

  @Mock
  private SectorRepository sectorRepository;

  @Override
  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    sectorService.init();
  }

  @Test
  public void createSector() {
    final Sector sector = buildMockSector();
    when(sectorRepository.existsById(SECTOR_ID)).thenReturn(Boolean.FALSE);
    when(sectorRepository.save(any(Sector.class))).thenReturn(sector);

    final Sector aux = sectorService.create(sector);

    assertEquals(aux.getName(), sector.getName());
    verify(sectorRepository).existsById(SECTOR_ID);
    verify(sectorRepository).save(sector);
  }

  @Test
  public void getEntityId() {
    final Sector sector = buildMockSector();

    assertEquals(SECTOR_ID, sectorService.getEntityId(sector));
  }

  @Test
  public void deleteFromTenant() {
    final String mockTenant = "mockTenantId";
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("tenantId", mockTenant);

    sectorService.deleteFromTenant(mockTenant);

    verifyDeleteFromFilter(filter, Sector.class);
  }

  @Test
  public void buildQueryForIdInCollection() {
    final Sector sector = buildMockSector();
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("id", Arrays.asList(sector.getId()));

    sectorService.delete(sector);

    verifyDeleteFromFilter(filter, Sector.class);
  }

  @Test
  public void delete() {
    final Sector sector_1 = mock(Sector.class);
    final Sector sector_2 = mock(Sector.class);
    final Sector[] aSectors = {sector_1, sector_2};
    when(sector_1.getId()).thenReturn("mock1");
    when(sector_2.getId()).thenReturn("mock2");

    sectorService.delete(Arrays.asList(aSectors));

    verify(mongoOps).remove(any(Query.class), eq(Sector.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(Provider.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(Application.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(Component.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(Sensor.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(Alert.class));
    verify(mongoOps).updateMulti(any(Query.class), any(Update.class), eq(User.class));

  }

  private Sector buildMockSector() {
    final Sector sector = new Sector(SECTOR_ID);
    sector.setName("testName");
    return sector;
  }

}
