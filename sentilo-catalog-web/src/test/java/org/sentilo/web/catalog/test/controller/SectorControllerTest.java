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
package org.sentilo.web.catalog.test.controller;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyCollection;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.web.catalog.controller.admin.SectorController;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.dto.GenericListDTO;
import org.sentilo.web.catalog.dto.GenericResponseDTO;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.SectorService;
import org.sentilo.web.catalog.service.UserService;
import org.springframework.context.MessageSource;
import org.springframework.validation.BindingResult;
import org.springframework.validation.support.BindingAwareModelMap;
import org.springframework.web.servlet.mvc.support.RedirectAttributesModelMap;

public class SectorControllerTest {

  private static final String SECTOR_NAME = "sector_name";
  private static final Object CREATE_SECTOR = "redirect:/admin/sector/list?nameTableRecover=sectorTable&sfbr=false";
  private static final String SECTOR_CREATE_PATH = "/admin/sector/create";

  @InjectMocks
  private SectorController controller;

  @Mock
  private BindingResult result;

  @Mock
  private HttpServletRequest request;

  @Mock
  private SectorService sectorService;

  @Mock
  private MessageSource messageSource;

  @Mock
  private UserService userService;

  private RedirectAttributesModelMap redirectAttributes;
  private BindingAwareModelMap model;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    redirectAttributes = new RedirectAttributesModelMap();
    model = new BindingAwareModelMap();
    model.put("activeMenu", "/sector");
  }

  @Test
  public void newSectorResource() {
    when(messageSource.getMessage(anyString(), any(Object[].class), any(Locale.class))).thenReturn("Test_MESSAGE");
    final String view = controller.newResource(request, model);
    Assert.assertEquals("sector/sector_new", view);
  }

  @Test
  public void createSector() {
    final Sector sector = buildSector();

    when(result.hasErrors()).thenReturn(false);
    when(sectorService.create(sector)).thenReturn(sector);
    when(request.getServletPath()).thenReturn(SECTOR_CREATE_PATH);
    final String view = controller.createResource(sector, result, model, redirectAttributes, request);

    Assert.assertEquals(CREATE_SECTOR, view);
  }

  @Test
  public void addUsers() {
    final String sectorId = "mock_sector";
    final String[] usersIds = {"user_1", "user_2", "user_3"};
    final GenericListDTO listDTO = new GenericListDTO();
    listDTO.setSelectedIds(usersIds);

    final GenericResponseDTO response = controller.addUsers(sectorId, listDTO, result, model);

    verify(userService).addUsersToSector(sectorId, usersIds);
    Assert.assertEquals(SentiloConstants.OK, response.getResult());
  }

  @Test
  public void addUsers_with_empty_array() {
    final String sectorId = "mock_sector";
    final String[] usersIds = {};
    final GenericListDTO listDTO = new GenericListDTO();
    listDTO.setSelectedIds(usersIds);

    final GenericResponseDTO response = controller.addUsers(sectorId, listDTO, result, model);

    verify(userService, times(0)).addUsersToSector(sectorId, usersIds);
    Assert.assertEquals(SentiloConstants.OK, response.getResult());
  }

  @Test
  public void checkIfLastSectorInUsers() {
    final String sectorId = "mock_sector";
    final String[] usersIds = {"user_1", "user_2", "user_3"};
    final GenericListDTO ids = new GenericListDTO();
    ids.setSelectedIds(usersIds);

    when(userService.findByIds(anyCollection())).thenReturn(buildAdminUsers(sectorId));

    final GenericListDTO users = controller.checkIfLastSectorInUsers(sectorId, ids, result, model);
    Assert.assertTrue(users.getSelectedIds().length == 1);

  }

  private Sector buildSector() {
    final Sector sector = new Sector();
    sector.setName(SECTOR_NAME);
    return sector;
  }

  private List<User> buildAdminUsers(final String sectorId) {
    final List<User> result = new ArrayList<>();
    final User usermock = new User("test");
    usermock.setRoles(Arrays.asList(Role.ADMIN));
    usermock.setSectors(Arrays.asList(sectorId));
    result.add(usermock);
    return result;
  }

}
