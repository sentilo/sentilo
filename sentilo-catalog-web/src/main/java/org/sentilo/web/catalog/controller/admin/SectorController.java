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
package org.sentilo.web.catalog.controller.admin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Sector;
import org.sentilo.web.catalog.domain.SectorResource.GrantType;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.dto.GenericListDTO;
import org.sentilo.web.catalog.dto.GenericResponseDTO;
import org.sentilo.web.catalog.dto.SectorGrantDTO;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.service.SectorService;
import org.sentilo.web.catalog.service.UserService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping("/admin/sector")
public class SectorController extends CrudController<Sector> {

  @Autowired
  private SectorService sectorService;

  @Autowired
  private UserService userService;

  @Autowired
  private ProviderService providerService;

  @Autowired
  private ApplicationService applicationService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_SECTOR;
  }

  @RequestMapping(value = "/{id}/addUsers", method = RequestMethod.GET)
  @PreAuthorize("@accessControlHandler.checkAccess(this, 'EDIT', #id)")
  public String getAddUsersView(@PathVariable final String id, final Model model) {
    model.addAttribute("sectorId", id);
    addResourceToModel(id, model);
    return Constants.VIEW_SECTOR_USERS_NOT_IN_LIST;
  }

  @RequestMapping(value = "/{id}/addUsers", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GenericResponseDTO addUsers(@PathVariable final String id, @RequestBody final GenericListDTO ids, final BindingResult result,
      final Model model) {

    if (!SentiloUtils.arrayIsEmpty(ids.getSelectedIds())) {
      userService.addUsersToSector(id, ids.getSelectedIds());
    }
    final String returnCode = result.hasErrors() ? SentiloConstants.KO : SentiloConstants.OK;
    final GenericResponseDTO response = new GenericResponseDTO(returnCode);
    result.getAllErrors().forEach(response::addErrorMessage);

    return response;
  }

  @RequestMapping(value = "/{id}/removeUsers", method = RequestMethod.POST)
  public String removeUsers(@PathVariable final String id, final GenericListDTO ids, final BindingResult result, final Model model,
      final RedirectAttributes redirectAttributes, final HttpServletRequest request) {

    if (!SentiloUtils.arrayIsEmpty(ids.getSelectedIds())) {
      userService.removeUsersFromSector(id, ids.getSelectedIds());
    }

    ModelUtils.addConfirmationMessageTo(redirectAttributes, "sector.users.removed");
    ModelUtils.addOpenedTabTo(redirectAttributes, Constants.TAB_2);
    addResourceToModel(id, model);
    return redirectToDetail(request);
  }

  @RequestMapping(value = "/{id}/checkIfLastSectorInUsers", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GenericListDTO checkIfLastSectorInUsers(@PathVariable final String id, @RequestBody final GenericListDTO ids, final BindingResult result,
      final Model model) {
    final List<User> users = userService.findByIds(Arrays.asList(ids.getSelectedIds()));
    final GenericListDTO response = new GenericListDTO();
    if (!CollectionUtils.isEmpty(users)) {
      final List<String> usersList = new ArrayList<>();
      users.stream().filter(user -> (user.getSectors().size() == 1 && id.equals(user.getSectors().get(0))))
          .forEach(user -> usersList.add(new StringBuilder(user.getId()).append("-").append(user.getName()).toString()));
      response.setSelectedIds(usersList.toArray(new String[usersList.size()]));
    }
    return response;
  }

  @RequestMapping(value = "/{id}/addProviders", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GenericResponseDTO addProviders(@PathVariable final String id, @RequestBody final SectorGrantDTO[] body, final BindingResult result,
      final Model model) {

    providerService.addProvidersToSector(id, convertRequestToGrantMap(body));
    final String returnCode = result.hasErrors() ? SentiloConstants.KO : SentiloConstants.OK;
    final GenericResponseDTO response = new GenericResponseDTO(returnCode);
    result.getAllErrors().forEach(response::addErrorMessage);

    return response;
  }

  @RequestMapping(value = "/{id}/removeProviders", method = RequestMethod.POST)
  public String removeProviders(@PathVariable final String id, final GenericListDTO ids, final BindingResult result, final Model model,
      final RedirectAttributes redirectAttributes, final HttpServletRequest request) {

    providerService.removeProvidersFromSector(id, Arrays.asList(ids.getSelectedIds()));
    ModelUtils.addConfirmationMessageTo(redirectAttributes, "sector.providers.removed");
    ModelUtils.addOpenedTabTo(redirectAttributes, Constants.TAB_3);
    addResourceToModel(id, model);
    return redirectToDetail(request);
  }

  @RequestMapping(value = "/{id}/addApplications", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GenericResponseDTO addApplications(@PathVariable final String id, @RequestBody final SectorGrantDTO[] body, final BindingResult result,
      final Model model) {

    applicationService.addApplicationsToSector(id, convertRequestToGrantMap(body));
    final String returnCode = result.hasErrors() ? SentiloConstants.KO : SentiloConstants.OK;
    final GenericResponseDTO response = new GenericResponseDTO(returnCode);
    result.getAllErrors().forEach(response::addErrorMessage);

    return response;
  }

  @RequestMapping(value = "/{id}/removeApplications", method = RequestMethod.POST)
  public String removeApplications(@PathVariable final String id, final GenericListDTO ids, final BindingResult result, final Model model,
      final RedirectAttributes redirectAttributes, final HttpServletRequest request) {

    applicationService.removeApplicationsFromSector(id, Arrays.asList(ids.getSelectedIds()));
    ModelUtils.addConfirmationMessageTo(redirectAttributes, "sector.applications.removed");
    ModelUtils.addOpenedTabTo(redirectAttributes, Constants.TAB_4);
    addResourceToModel(id, model);
    return redirectToDetail(request);
  }

  @Override
  protected Sector buildNewEntity(final String id) {
    return new Sector(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_SECTOR;
  }

  @Override
  protected CrudService<Sector> getService() {
    return sectorService;
  }

  @Override
  protected List<String> toRow(final Sector sector) {
    final List<String> row = new ArrayList<String>();
    row.add(sector.getId()); // checkbox
    row.add(sector.getId());
    row.add(sector.getName());
    row.add(getLocalDateFormat().printAsLocalTime(sector.getCreatedAt(), Constants.DATETIME_FORMAT));
    row.add(sector.getCreatedBy());
    return row;
  }

  @Override
  protected List<String> toExcelRow(final Sector sector) {
    return ExcelGeneratorUtils.getSectorExcelRowsData(sector, getLocalDateFormat());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_SECTOR_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_SECTOR_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_SECTOR);
  }

  @Override
  protected void doBeforeViewResource(final String id, final Model model) {
    super.doBeforeViewResource(id, model);
    model.addAttribute(Constants.SECTOR_READ_PERMISSIONS_TYPE, GrantType.R);
    model.addAttribute(Constants.SECTOR_ADMIN_PERMISSIONS_TYPE, GrantType.A);
  }

  /**
   * Converts the array of {@link SectorGrantDTO} in a Map<String, GrantType>
   * 
   * @param permissions - The array of {@link SectorGrantDTO}
   * @return a Map<String, GrantType>
   */
  private Map<String, GrantType> convertRequestToGrantMap(final SectorGrantDTO[] permissions) {
    final Map<String, GrantType> grantedMap = new HashMap<>();
    Arrays.stream(permissions).forEach(sp -> grantedMap.put(sp.getSelectedId(), GrantType.valueOf(sp.getGrant())));
    return grantedMap;
  }

}
