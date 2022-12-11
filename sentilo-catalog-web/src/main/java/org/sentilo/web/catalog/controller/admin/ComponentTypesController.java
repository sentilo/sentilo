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
package org.sentilo.web.catalog.controller.admin;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.ComponentService;
import org.sentilo.web.catalog.service.ComponentTypesService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ExcelGeneratorUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/admin/componenttypes")
public class ComponentTypesController extends CrudController<ComponentType> {

  @Autowired
  private ComponentTypesService componentTypesService;

  @Autowired
  private ComponentService componentService;

  @Autowired
  private ServletContext context;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_COMPONENT_TYPE;
  }

  @Override
  protected ComponentType buildNewEntity(final String id) {
    return new ComponentType(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_COMPONENT_TYPE;
  }

  @Override
  protected CrudService<ComponentType> getService() {
    return componentTypesService;
  }

  @Override
  protected List<String> toRow(final ComponentType componentType) {
    final List<String> row = new ArrayList<String>();
    row.add(componentType.getId()); // checkbox
    row.add(componentType.getId());
    row.add(componentType.getName());
    row.add(componentType.getDescription());
    row.add(getLocalDateFormat().printAsLocalTime(componentType.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final ComponentType componentType) {
    return ExcelGeneratorUtils.getComponentTypesExcelRowsData(componentType, getLocalDateFormat());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_COMPONENT_TYPE_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_COMPONENT_TYPE_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_COMPONENT_TYPE);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeNewResource(javax.servlet.http.
   * HttpServletRequest, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeNewResource(final HttpServletRequest request, final Model model) {
    super.doBeforeNewResource(request, model);

    setIconsListToModel(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeEditResource(java.lang.String,
   * org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeEditResource(final String id, final Model model) {
    super.doBeforeEditResource(id, model);

    setIconsListToModel(model);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.controller.CrudController#doBeforeDeleteResources(java.util.Collection,
   * javax.servlet.http.HttpServletRequest, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeDeleteResources(final Collection<ComponentType> resources, final HttpServletRequest request, final Model model) {
    super.doBeforeDeleteResources(resources, request, model);

    for (final ComponentType componentType : resources) {
      throwExceptionIfComponentsFoundWithType(componentType.getId());
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeCreateResource(org.sentilo.web.
   * catalog .domain.CatalogDocument, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeCreateResource(final ComponentType componentType, final Model model) {
    super.doBeforeCreateResource(componentType, model);

    // Id is stored in lower case to do comparatives in a quickly way
    componentType.setId(componentType.getId().toLowerCase());
  }

  private void setIconsListToModel(final Model model) {
    // To retrieve the icons list is mandatory that application will be deployed as an exploded WAR
    // in Tomcat server
    String[] iconsNames = {};
    try {
      final String absoluteIconsPath = context.getRealPath("/static/img/icons/");
      final File iconsDir = new File(absoluteIconsPath);
      iconsNames = iconsDir.list(new FilenameFilter() {

        @Override
        public boolean accept(final File directory, final String fileName) {
          return !fileName.endsWith("-poi.png") && !fileName.startsWith("poi-group");
        }
      });
    } catch (final Exception e) {
    }

    model.addAttribute(Constants.MODEL_COMPONENT_TYPE_ICONS, Arrays.asList(iconsNames));
  }

  private void throwExceptionIfComponentsFoundWithType(final String componentType) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("componentType", componentType);
    final SearchFilterResult<Component> components = componentService.search(filter);
    if (!CollectionUtils.isEmpty(components.getContent())) {
      throw new BusinessValidationException("componenttype.error.cannot.delete", new Object[] {componentType});
    }
  }

  @RequestMapping(value = "/search/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public List<ComponentType> search(final HttpServletRequest request, @RequestParam(required = true) final String providerId, final Model model) {
    return componentTypesService.findComponentTypesByProvider(providerId);
  }

}
