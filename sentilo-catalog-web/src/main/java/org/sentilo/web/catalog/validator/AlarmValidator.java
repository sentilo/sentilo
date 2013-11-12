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
package org.sentilo.web.catalog.validator;

import org.sentilo.web.catalog.domain.Alarm;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.service.SensorService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;


@ValidatorComponent
public class AlarmValidator implements Validator {

	@Autowired
	private SensorService sensorService;

	@Override
	public boolean supports(Class<?> clazz) {
		return Alarm.class.equals(clazz);
	}

	@Override
	public void validate(Object target, Errors errors) {
		Alarm alarm = (Alarm) target;

		if (alarm.getType() == Alarm.Type.INTERNAL) {
			validateInternalAlarm(alarm, errors);
		} else if (alarm.getType() == Alarm.Type.EXTERNAL) {
			validateExternalAlarm(alarm, errors);
		}
	}

	private void validateExternalAlarm(Alarm alarm, Errors errors) {
		if (!StringUtils.hasText(alarm.getClientApplication())) {
			errors.rejectValue("clientApplication", "NotBlank");
		}
	}

	private void validateInternalAlarm(Alarm alarm, Errors errors) {
		if (!StringUtils.hasText(alarm.getSensorId())) {
			errors.rejectValue("sensorId", "NotBlank");
		}
		
		if (!StringUtils.hasText(alarm.getProviderId())) {
			errors.rejectValue("providerId", "NotBlank");
		}
		
		if (StringUtils.hasText(alarm.getProviderId()) && StringUtils.hasLength(alarm.getSensorId())) {
			Sensor sensor = sensorService.find(new Sensor(alarm.getProviderId(), alarm.getComponentId(), alarm.getSensorId()));
			if (sensor == null) {
				errors.rejectValue("sensorId", "alarm.error.sensor.notfound");
			}
		}
		
		validateTriggerType(alarm, errors);
						
	}
	
	private void validateTriggerType(Alarm alarm, Errors errors){
		//Validacions associades al tipus de trigger i l'expressió informada
		switch(alarm.getTrigger()){
			case GT:
			case GTE:
			case LT:
			case LTE: 
			case CHANGE_DELTA:
			case FROZEN:	
				validateIfExpressionValueIsEmpty(alarm, errors);
				validateIfExpressionValueIsNumeric(alarm, errors);
				break;
			case EQ:
				validateIfExpressionValueIsEmpty(alarm, errors);
				break;
			default:
				break;
		}
	}
	
	private void validateIfExpressionValueIsEmpty(Alarm alarm, Errors errors){
		if(!StringUtils.hasText(alarm.getExpression())){
			errors.rejectValue("expression", "NotBlank");
		}
	}
	
	private void validateIfExpressionValueIsNumeric(Alarm alarm, Errors errors){		
		if(StringUtils.hasText(alarm.getExpression()) && !alarm.getExpression().matches("[-+]?\\d+(\\.\\d+)?")){
			errors.rejectValue("expression", "NotNumber");
		}
	}
}
