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
package org.sentilo.web.catalog.utils;

import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.domain.ComponentType;
import org.springframework.util.StringUtils;

public abstract class ComponentUtils extends CatalogUtils {

  protected ComponentUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }

  public static String compileExtendedDetailUrl(final Component component, final ComponentType componentType) {
    String url = component.getExtendedDetailUrl();
    if (!StringUtils.hasText(url) && componentType != null) {
      url = componentType.getExtendedDetailUrl();
    }
    final String targetUrl = replaceVariablesInUrl(url, component);
    component.setExtendedDetailUrl(targetUrl);
    return targetUrl;
  }

  public static String compilePhotoUrl(final Component component, final ComponentType componentType) {
    String url = component.getPhotoUrl();
    if (!StringUtils.hasText(url) && componentType != null) {
      url = componentType.getPhotoUrl();
    }

    final String targetUrl = replaceVariablesInUrl(url, component);
    component.setPhotoUrl(targetUrl);
    return targetUrl;
  }

  private static String replaceVariablesInUrl(final String sourceUrl, final Component component) {
    String aux = sourceUrl;
    if (StringUtils.hasText(aux)) {
      aux = StringUtils.replace(aux, "${component_id}", component.getName());
      aux = StringUtils.replace(aux, "${provider_id}", component.getProviderId());
      aux = StringUtils.replace(aux, "${tenant_id}", component.getTenantId());
    }
    return aux;
  }
}
