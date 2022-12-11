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
package org.sentilo.web.catalog.test.view;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.view.ExcelBuilder;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

public class ExcelBuilderTest {

  @Mock
  private MessageSource messageSource;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private ServletOutputStream outputStream;

  @Mock
  private Map<String, Object> model;

  @InjectMocks
  private ExcelBuilder builder;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void render() throws Exception {
    final List<String> values = Arrays.asList("value1", "value2", "<span>spanValue3</span>");
    final List<List<String>> resourceList = Arrays.asList(values);

    final List<String> columKeySuffixes = Arrays.asList("colum1", "column2", "column3");
    final Map<String, Object> model = new HashMap<String, Object>();
    model.put(Constants.MESSAGE_KEYS_PREFIX, "mockPrefix");
    model.put(Constants.LIST_COLUMN_NAMES, columKeySuffixes);
    model.put(Constants.RESULT_LIST, resourceList);

    when(messageSource.getMessage(any(String.class), any(Object[].class), eq(LocaleContextHolder.getLocale()))).thenReturn("mockMesage");
    when(response.getOutputStream()).thenReturn(outputStream);

    builder.render(model, request, response);

    verify(response).setContentType("application/vnd.ms-excel");

  }

}
