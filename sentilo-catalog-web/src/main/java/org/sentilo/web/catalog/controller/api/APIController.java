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
package org.sentilo.web.catalog.controller.api;

import java.util.Iterator;
import java.util.Map;

import org.sentilo.web.catalog.search.SearchFilter;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.util.StringUtils;


public abstract class APIController {
	
	protected SearchFilter buildSearchFilter(Pageable pageable, String search) {

		Pageable translated = translateSortFields(pageable);
		if (StringUtils.hasText(search)) {
			Map<String, String> params = buildSearchParams(search);
			return new SearchFilter(params, translated);
		}
		return new SearchFilter(translated);
	}

	/**
	 * Este método construye los parámetros de búsqueda necesarios para poder
	 * filtrar datos en los listados.
	 * 
	 * @param search
	 *            Contenido del input filter de los listados.
	 * @return
	 */
	protected abstract Map<String, String> buildSearchParams(String search);

	/**
	 * Este método tiene que devolver la lista de campos que se está mostrando
	 * en el listado. Nota: Los campos deben coincidir con los que hay en
	 * mongodb
	 * 
	 * @return
	 */
	protected abstract String[] getFieldNames();

	/**
	 * Este método traduce el número de los campos que proporciona dataTables en
	 * el nombre que posteriormente usará la capa de datos para la ordenación.
	 * 
	 * @param original
	 * @return
	 */
	private Pageable translateSortFields(Pageable original) {

		int pageNumber = original.getPageNumber();
		int pageSize = original.getPageSize();

		if (original.getSort() != null) {
			Order order = getFirstOrder(original);
			if (order != null) {
				return new PageRequest(pageNumber, pageSize, order.getDirection(), translateProperty(order.getProperty()));
			}
		}
		return new PageRequest(pageNumber, pageSize);
	}

	private Order getFirstOrder(Pageable original) {
		Iterator<Order> it = original.getSort().iterator();
		if (it.hasNext()) {
			return it.next();
		}
		return null;
	}

	private String translateProperty(String property) {
		try {
			String[] fieldNames = getFieldNames();
			int value = Integer.parseInt(property);

			if (value < fieldNames.length) {
				return fieldNames[value];
			}
		} catch (NumberFormatException unmanaged) {
		}
		throw new IllegalArgumentException("Cannot translate " + property + " into field name");
	}
}
