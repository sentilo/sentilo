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
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.controller.CrudController;
import org.sentilo.web.catalog.domain.Alarm;
import org.sentilo.web.catalog.domain.Alarm.Type;
import org.sentilo.web.catalog.service.AlarmService;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.service.ProviderService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.FormatUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;


@Controller
@RequestMapping("/admin/alarm")
public class AlarmController extends CrudController<Alarm> {

	@Autowired
	private AlarmService alarmService;

	@Autowired
	private ApplicationService applicationService;

	@Autowired
	private ProviderService providerService;

	@Autowired
	private MessageSource messageSource;

	@ModelAttribute(Constants.MODEL_ACTIVE_MENU)
	public String getActiveMenu() {
		return Constants.MENU_ALARM;
	}

	protected void initViewNames() {
		viewNames.put(LIST_ACTION, Constants.VIEW_ALARM_LIST);
		viewNames.put(DETAIL_ACTION, Constants.VIEW_ALARM_DETAIL);
		viewNames.put(NEW_ACTION, Constants.VIEW_NEW_ALARM);
	}

	@Override
	protected List<String> toRow(Alarm alarm) {
		List<String> row = new ArrayList<String>();
		row.add(alarm.getId()); //checkbox
		row.add(alarm.getId());
		row.add(alarm.getName());
		row.add(FormatUtils.label(messageSource.getMessage("alarm.type." + alarm.getType().toString(), null, Locale.getDefault())));
		row.add(FormatUtils.formatDate(alarm.getCreatedAt()));
		return row;
	}

	private void addAlarmMasterDataTo(Model model) {
		addProviderListTo(model);
		addApplicationListTo(model);		
		addAlarmTypesTo(model);
		addAlarmTriggersTo(model);
	}

	private void addProviderListTo(Model model) {
		model.addAttribute(Constants.MODEL_PROVIDERS, providerService.findAll());
	}
	
	private void addApplicationListTo(Model model) {
		model.addAttribute(Constants.MODEL_APPLICATIONS, applicationService.findAll());
	}

	private void addAlarmTypesTo(Model model) {
		model.addAttribute(Constants.MODEL_ALARM_TYPES, Alarm.Type.values());
	}

	private void addAlarmTriggersTo(Model model) {
		model.addAttribute(Constants.MODEL_ALARM_TRIGGERS, Alarm.Trigger.values());
	}

	@Override
	protected CrudService<Alarm> getService() {
		return alarmService;
	}

	@Override
	protected Alarm buildNewEntity(String id) {
		return new Alarm(id);
	}

	@Override
	protected String getEntityModelKey() {
		return Constants.MODEL_ALARM;
	}

	@Override
	protected void doBeforeNewResource(HttpServletRequest request, Model model) {
		addAlarmMasterDataTo(model);
	}

	@Override
	protected void doBeforeEditResource(Model model) {
		addAlarmMasterDataTo(model);
	}
	
	@Override
	protected void doBeforeCreateResource(Alarm alarm, Model model){
		// Si la alarma es interna, forzamos a null los atributos especificos de una alarma externa.
		// Ídem con alarma externa
		
		if(Type.INTERNAL.equals(alarm.getType())){
			alarm.setClientApplication(null);
		}else{
			alarm.setProviderId(null);
			alarm.setComponentId(null);
			alarm.setSensorId(null);
			alarm.setTrigger(null);
			alarm.setExpression(null);			
		}
	}
}
