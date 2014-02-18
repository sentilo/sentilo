/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.common.test.utils;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.URIUtils;

public class URIUtilsTest {

  static final String HOST = "http://www.example.com";
  static final String HOST_ENDS_WITH_SLASH = "http://www.example.com/";
  static final String INVALID_HOST = "http:\\www.example.com";
  static final String PATH = "/service/endpoint";
  static final String PATH_NOT_STARTS_WITH_SLASH = "/service/endpoint";

  @Test
  public void uriWithoutParams() {
    final String uri = URIUtils.getURI(HOST, PATH);
    assertEquals("http://www.example.com/service/endpoint", uri);
  }

  @Test
  public void uriWithHostAndPathWithSlash() {
    final String uri = URIUtils.getURI(HOST_ENDS_WITH_SLASH, PATH);
    assertEquals("http://www.example.com/service/endpoint", uri);
  }

  @Test
  public void uriWithoutHostAndPathWithSlash() {
    final String uri = URIUtils.getURI(HOST, PATH_NOT_STARTS_WITH_SLASH);
    assertEquals("http://www.example.com/service/endpoint", uri);
  }

  @Test
  public void uriWithParams() {
    final RequestParameters parameters = new RequestParameters();
    parameters.put("param1", "value1");
    parameters.put("param2", "value2");

    final String uri = URIUtils.getURI(HOST, PATH, parameters);

    assertEquals("http://www.example.com/service/endpoint?param1=value1&param2=value2", uri);
  }

  @Test
  public void uriWithEmptyParams() {
    final RequestParameters parameters = new RequestParameters();
    assertEquals(URIUtils.getURI(HOST, PATH), URIUtils.getURI(HOST, PATH, parameters));
  }

  @Test(expected = IllegalArgumentException.class)
  public void uriWithNullHost() {
    URIUtils.getURI(null, PATH);
  }

  @Test(expected = IllegalArgumentException.class)
  public void uriWithInvalidFormatHost() {
    URIUtils.getURI(INVALID_HOST, PATH);
  }
}
