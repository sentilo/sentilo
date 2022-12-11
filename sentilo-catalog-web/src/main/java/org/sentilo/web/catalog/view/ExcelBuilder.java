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
package org.sentilo.web.catalog.view;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellUtil;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.view.document.AbstractXlsView;

@Component
public class ExcelBuilder extends AbstractXlsView {

  @Autowired
  private MessageSource messageSource;

  @SuppressWarnings("unchecked")
  @Override
  protected void buildExcelDocument(final Map<String, Object> model, final Workbook workbook, final HttpServletRequest request,
      final HttpServletResponse response) throws Exception {

    final List<List<String>> resourceList = (List<List<String>>) model.get(Constants.RESULT_LIST);
    final List<String> columnsKeys = buildColumnKeys(model);

    final boolean ignoreFirstValue = !CollectionUtils.isEmpty(resourceList) && columnsKeys.size() != resourceList.get(0).size();

    final Sheet sheet = workbook.createSheet("list");

    final Row header = sheet.createRow(0);
    final CellStyle style = workbook.createCellStyle();
    final Font font = workbook.createFont();
    font.setBold(true);
    style.setFont(font);
    // Call method to put the Column names Headers
    getHeaderExcel(header, style, columnsKeys);
    int i = 0;
    for (final List<String> rowValues : resourceList) {
      final Row row = sheet.createRow(++i);
      // put the content in the rows
      toExcelRow(rowValues, row, ignoreFirstValue);
    }

    for (int j = 0; j <= columnsKeys.size(); j++) {
      sheet.autoSizeColumn(j);
    }
  }

  private void toExcelRow(final List<String> rowValues, final Row row, final boolean ignoreFirstValue) {
    int column = 0;
    for (int i = ignoreFirstValue ? 1 : 0; i < rowValues.size(); i++) {
      final String value = rowValues.get(i);
      CellUtil.createCell(row, column++, cleanLabelwrapper(value));
    }
  }

  private String cleanLabelwrapper(final String value) {
    // Some values are HTML <span> content so before we add them into the cell this HTML code must
    // be removed
    String cleanValue = StringUtils.hasText(value) ? value : "";

    if (StringUtils.hasText(cleanValue) && value.endsWith("</span>")) {
      final int begin = cleanValue.indexOf(">");
      final int end = cleanValue.lastIndexOf("<");
      cleanValue = cleanValue.substring(begin + 1, end);
    }

    return cleanValue;
  }

  @SuppressWarnings("unchecked")
  private List<String> buildColumnKeys(final Map<String, Object> model) {
    final List<String> columnKeys = new ArrayList<String>();
    final List<String> columKeySuffixes = (List<String>) model.get(Constants.LIST_COLUMN_NAMES);
    final String keysPreffix = (String) model.get(Constants.MESSAGE_KEYS_PREFIX);

    Assert.hasLength(keysPreffix, "Message keys preffix must not be empty");

    for (final String columnKeySuffix : columKeySuffixes) {
      columnKeys.add(keysPreffix + Constants.DEFAULT_KEY_TOKEN_SPLITTER + columnKeySuffix);
    }

    return columnKeys;
  }

  private void getHeaderExcel(final Row header, final CellStyle style, final List<String> columnsKeys) {
    int i = 0;
    final Locale locale = LocaleContextHolder.getLocale();
    for (final String columnKey : columnsKeys) {
      final Cell cellHeader = header.createCell(i++);
      cellHeader.setCellStyle(style);
      cellHeader.setCellValue(messageSource.getMessage(columnKey, null, locale).toUpperCase(locale));
    }
  }
}
