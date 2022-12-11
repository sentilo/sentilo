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
package org.sentilo.web.catalog.test.context;

import static org.mockito.Mockito.when;

import javax.servlet.http.HttpServletRequest;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.context.DataTablesContextHolder;
import org.sentilo.web.catalog.context.DataTablesContextImpl;
import org.sentilo.web.catalog.utils.Constants;

public class DataTablesContextImplTest {

  final String mockKey = "mockKey";
  final String mockValue = "1234";
  final String tableName = "mock_list";

  @Mock
  private HttpServletRequest request;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(request.getParameter(mockKey)).thenReturn(mockValue);
    when(request.getParameter(Constants.TABLE_NAME_REQUEST_PARAM)).thenReturn(tableName);
  }

  @Test
  public void newContext() {
    final DataTablesContextImpl context = new DataTablesContextImpl(request);

    Assert.assertEquals(tableName, context.getTableName());
    Assert.assertEquals(mockValue, context.getParam(mockKey));
  }

  @Test
  public void dataTablesContextHolder() {
    final String tableName = "mock_list";
    final DataTablesContextImpl context = new DataTablesContextImpl(request);
    DataTablesContextHolder.setContext(context);

    Assert.assertTrue(DataTablesContextHolder.hasContext());
    Assert.assertEquals(tableName, DataTablesContextHolder.getContext().getTableName());

    DataTablesContextHolder.clearContext();

    Assert.assertFalse(DataTablesContextHolder.hasContext());
  }
}
