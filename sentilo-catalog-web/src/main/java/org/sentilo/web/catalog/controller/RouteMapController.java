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
package org.sentilo.web.catalog.controller;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.dto.MapComponentDTO;
import org.sentilo.web.catalog.dto.MapModelDTO;
import org.sentilo.web.catalog.dto.MapRouteComponentDTO;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.ModelUtils;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/component/map/route")
public class RouteMapController extends AbstractMapController {

  @ModelAttribute(Constants.MODEL_CURRENT_REQUEST_MAPPING)
  public String getCurrentRequestMapping() {
    return "/component/map/route";
  }

  @RequestMapping(value = "", method = RequestMethod.GET)
  public String showMap(final Model model, final Principal principal) {
    ModelUtils.addActiveMenuTo(model, Constants.MENU_COMPONENT_MAP);
    model.addAttribute(Constants.MODEL_DATE_UPDATED, getLocalDateFormat().printCurrentAsLocalTime());
    // Component types's list is needed to render the page, because a select is displayed
    // to filter components to visualize.
    final boolean onlyPublics = principal == null ? true : false;
    model.addAttribute(Constants.MODEL_COMPONENT_TYPES, getComponentTypesService().getActiveComponentTypes(onlyPublics));

    model.addAttribute(Constants.MODEL_MAP_TYPE, "route");
    return Constants.VIEW_PUBLIC_ROUTE_MAP;
  }

  @RequestMapping(value = "/json", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public MapModelDTO getJSONComponentMap(@RequestParam(required = false) final String componentType,
      @RequestParam(required = false) final String[] bounds, final Principal principal, final Model model) {

    final String[] componentTypes = StringUtils.hasText(componentType) ? new String[] {componentType} : null;
    final SearchFilter filter = buildSearchFilter(componentTypes, bounds);

    final Map<String, String> icons = getIconMap();
    final List<MapComponentDTO> result = new ArrayList<MapComponentDTO>();
    for (final Component component : getComponentService().geoSpatialSearch(filter).getContent()) {
      if (component.getRoutePointList() != null) {
        result.add(new MapRouteComponentDTO(component, icons.get(component.getComponentType())));
      }
    }
    return new MapModelDTO(result);
  }
}
