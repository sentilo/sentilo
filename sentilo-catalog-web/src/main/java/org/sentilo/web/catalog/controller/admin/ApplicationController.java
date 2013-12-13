/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.controller.admin;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Alarm;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.exception.BusinessValidationException;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.AlarmService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;


@Controller
@RequestMapping("/admin/application")
public class ApplicationController extends CrudController<Application> {

	@Autowired
	private ApplicationService applicationService;

	@Autowired
	private AlarmService alarmService;

	@ModelAttribute(Constants.MODEL_ACTIVE_MENU)
	public String getActiveMenu() {
		return Constants.MENU_APPLICATION;
	}

	@Override
	protected List<String> toRow(Application application) {
		List<String> row = new ArrayList<String>();
		row.add(application.getId()); //checkbox
		row.add(application.getId());
		row.add(application.getName());
		row.add(application.getDescription());
		row.add(FormatUtils.formatDate(application.getCreatedAt()));
		return row;
	}

	protected void initViewNames() {
		viewNames.put(LIST_ACTION, Constants.VIEW_APPLICATION_LIST);
		viewNames.put(DETAIL_ACTION, Constants.VIEW_APPLICATION_DETAIL);
		viewNames.put(NEW_ACTION, Constants.VIEW_NEW_APPLICATION);
	}

	@Override
	protected void doBeforeDeleteResource(String[] selectedIds, HttpServletRequest request, Model model) {
		throwExceptionIfAlarmFound(selectedIds);
	}

	@Override
	protected CrudService<Application> getService() {
		return applicationService;
	}

	@Override
	protected Application buildNewEntity(String id) {
		return new Application(id);
	}

	@Override
	protected String getEntityModelKey() {
		return Constants.MODEL_APPLICATION;
	}

	private void throwExceptionIfAlarmFound(String[] selectedIds) {
		for (String applicationId : selectedIds) {
			SearchFilter filter = new SearchFilter();
			filter.addParam("type", Alarm.Type.EXTERNAL.toString());
			filter.addAndParam("applicationId", applicationId);
			boolean alarmFound = alarmService.search(filter).getContent().size() > 0;
			if (alarmFound) {
				throw new BusinessValidationException("application.error.existing.alarms");
			}
		}
	}
}
