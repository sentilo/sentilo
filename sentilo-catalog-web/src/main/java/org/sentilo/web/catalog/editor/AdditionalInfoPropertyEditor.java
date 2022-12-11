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
package org.sentilo.web.catalog.editor;

import java.beans.PropertyEditorSupport;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

public class AdditionalInfoPropertyEditor extends PropertyEditorSupport {

  private static final Logger LOGGER = LoggerFactory.getLogger(AdditionalInfoPropertyEditor.class);

  @Override
  public void setAsText(final String text) {
    if (StringUtils.hasText(text)) {
      setValue(jsonStrToMap(text));
    }
  }

  private Map<String, String> jsonStrToMap(final String text) {
    Map<String, String> map = null;
    final ObjectMapper mapper = new ObjectMapper();
    try {
      final List<AdditionalInfoEntry> aux = Arrays.asList(mapper.readValue(text, AdditionalInfoEntry[].class));
      map = aux.stream().collect(Collectors.toMap(AdditionalInfoEntry::getKey, AdditionalInfoEntry::getValue, (k1, k2) -> k1));
    } catch (final Exception e) {
      LOGGER.warn("Error deserializing json text {} . Original value is not modified. ", text, e);
    }

    return map;
  }

  public static class AdditionalInfoEntry {

    private String key;
    private String value;

    public AdditionalInfoEntry() {
      super();
    }

    public String getKey() {
      return key;
    }

    public void setKey(final String key) {
      this.key = key;
    }

    public String getValue() {
      return value;
    }

    public void setValue(final String value) {
      this.value = value;
    }

  }
}
