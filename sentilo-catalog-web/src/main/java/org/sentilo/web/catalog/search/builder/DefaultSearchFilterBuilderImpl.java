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
package org.sentilo.web.catalog.search.builder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.sentilo.web.catalog.search.SearchFilter;
import org.springframework.data.domain.Pageable;
import org.springframework.util.StringUtils;


public class DefaultSearchFilterBuilderImpl implements SearchFilterBuilder {

	public DefaultSearchFilterBuilderImpl() {
		registerDataTableColumns();
	}

	@Override
	public SearchFilter buildSearchFilter(HttpServletRequest request, Pageable pageable, String wordToSearch) {
		if (StringUtils.hasText(wordToSearch)) {
			Map<String, String> params = buildSearchParams(request, wordToSearch);
			return new SearchFilter(params, pageable);
		}
		return new SearchFilter(pageable);
	}

	@Override
	public void registerDataTableColumns() {
		registerAlarmDataTableColumns();
		registerProviderDataTableColumns();
		registerApplicationDataTableColumns();
		registerSensorDataTableColumns();
		registerComponentDataTableColumns();
		registerUserDataTableColumns();
		registerPermissionsDataTableColumns();
		registerSensorTypesDataTableColumns();
		registerComponentTypesDataTableColumns();
	}

	protected Map<String, String> buildSearchParams(HttpServletRequest request, String wordToSearch) {
		Map<String, String> searchParams = new HashMap<String, String>();
		List<Column> listColumns = SearchFilterUtils.getListColumns(request);

		for (Column column : listColumns) {
			if (column.isSearchable()) {
				searchParams.put(column.getName(), wordToSearch);
			}
		}

		return searchParams;
	}

	private void registerAlarmDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("type", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("alarm", columns);
	}

	private void registerProviderDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("description", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("provider", columns);
	}

	private void registerApplicationDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("description", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("application", columns);
	}

	private void registerSensorDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("sensorId", true, true));
		columns.add(new Column("providerId", true, true));
		columns.add(new Column("type", true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("sensor", columns);
	}

	private void registerComponentDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("description", true, true));
		columns.add(new Column("mobile", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("component", columns);
	}

	private void registerUserDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("userName", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("email", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("users", columns);
	}

	private void registerPermissionsDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("target", true, true));
		columns.add(new Column("type", true));

		SearchFilterUtils.addListColumns("permissions", columns);
	}

	private void registerSensorTypesDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("description", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("sensortypes", columns);
	}

	private void registerComponentTypesDataTableColumns() {
		List<Column> columns = new ArrayList<Column>();
		columns.add(new Column("_id", true, true));
		columns.add(new Column("name", true, true));
		columns.add(new Column("description", true, true));
		columns.add(new Column("createdAt", true));

		SearchFilterUtils.addListColumns("componenttypes", columns);
	}

}
