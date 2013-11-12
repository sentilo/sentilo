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

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;


@ValidatorComponent
public class CoordinatesValidator implements Validator {

	@Override
	public boolean supports(Class<?> clazz) {
		return Component.class.equals(clazz) || Sensor.class.equals(clazz);
	}

	@Override
	public void validate(Object target, Errors errors) {

		if (target instanceof Component) {
			Component component = (Component) target;
			if (component.isStaticComponent()) {
				validateLocation(component.getLocation(), errors);
			}
			return;
		}
	}

	private void validateLocation(Location target, Errors errors) {

		fixDecimalSeparator(target);

		validateRequiredDouble(target.getLatitude(), "location.latitude", errors);
		validateRequiredDouble(target.getLongitude(), "location.longitude", errors);
	}

	private void validateRequiredDouble(String target, String property, Errors errors) {
		if (!StringUtils.hasText(target)) {
			errors.rejectValue(property, "NotBlank");
			return;
		}
		try {
			Double.parseDouble(target);
		} catch (NumberFormatException e) {
			errors.rejectValue(property, "NotValidDouble");
		}
	}

	private void fixDecimalSeparator(Location target) {
		target.setLatitude(fixDecimalSeparator(target.getLatitude()));
		target.setLongitude(fixDecimalSeparator(target.getLongitude()));
	}

	/**
	 * Convierte el separador decimal de coma a punto.
	 * 
	 * @param target
	 * @return
	 */
	private String fixDecimalSeparator(String target) {
		return target.replace(',', '.');
	}
}
