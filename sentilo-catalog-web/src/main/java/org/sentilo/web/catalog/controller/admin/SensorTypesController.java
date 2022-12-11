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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.SensorType;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.SensorService;
import org.sentilo.web.catalog.service.SensorTypesService;
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
@RequestMapping("/admin/sensortypes")
public class SensorTypesController extends CrudController<SensorType> {

  @Autowired
  private SensorTypesService sensorTypesService;

  @Autowired
  private SensorService sensorService;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_SENSOR_TYPE;
  }

  @Override
  protected SensorType buildNewEntity(final String id) {
    return new SensorType(id);
  }

  @Override
  protected String getEntityModelKey() {
    return Constants.MODEL_SENSOR_TYPE;
  }

  @Override
  protected CrudService<SensorType> getService() {
    return sensorTypesService;
  }

  @Override
  protected List<String> toRow(final SensorType sensorType) {
    final List<String> row = new ArrayList<String>();
    row.add(sensorType.getId()); // checkbox
    row.add(sensorType.getId());
    row.add(sensorType.getName());
    row.add(sensorType.getDescription());
    row.add(getLocalDateFormat().printAsLocalTime(sensorType.getCreatedAt(), Constants.DATETIME_FORMAT));
    return row;
  }

  @Override
  protected List<String> toExcelRow(final SensorType sensorType) {
    return ExcelGeneratorUtils.getSensorTypesExcelRowsData(sensorType, getLocalDateFormat());
  }

  @Override
  protected void initViewNames() {
    getViewNames().put(LIST_ACTION, Constants.VIEW_SENSOR_TYPE_LIST);
    getViewNames().put(DETAIL_ACTION, Constants.VIEW_SENSOR_TYPE_DETAIL);
    getViewNames().put(NEW_ACTION, Constants.VIEW_NEW_SENSOR_TYPE);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.controller.CrudController#doBeforeDeleteResources(java.util.Collection,
   * javax.servlet.http.HttpServletRequest, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeDeleteResources(final Collection<SensorType> sensorTypes, final HttpServletRequest request, final Model model) {
    super.doBeforeDeleteResources(sensorTypes, request, model);

    for (final SensorType sensorType : sensorTypes) {
      throwExceptionIfSensorsFoundWithType(sensorType.getId());
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.controller.CrudController#doBeforeCreateResource(org.sentilo.web.
   * catalog .domain.CatalogDocument, org.springframework.ui.Model)
   */
  @Override
  protected void doBeforeCreateResource(final SensorType resource, final Model model) {
    super.doBeforeCreateResource(resource, model);

    // Id is stored in lower case to do comparatives in a quickly way
    resource.setId(resource.getId().toLowerCase());
  }

  private void throwExceptionIfSensorsFoundWithType(final String sensorType) {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("type", sensorType);
    final SearchFilterResult<Sensor> sensors = sensorService.search(filter);
    if (!CollectionUtils.isEmpty(sensors.getContent())) {
      throw new BusinessValidationException("sensortype.error.cannot.delete", new Object[] {sensorType});
    }
  }

  @RequestMapping(value = "/search/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public List<SensorType> search(final HttpServletRequest request, @RequestParam(required = true) final String providerId, final Model model) {
    return sensorTypesService.findSensorTypesByProvider(providerId);
  }

}
