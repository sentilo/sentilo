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
package org.sentilo.common.test.rest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;

import org.junit.Test;
import org.sentilo.common.rest.RequestParameters;
import org.sentilo.common.utils.SentiloConstants;

public class RequestParametersTest {

  @Test
  public void buildEmptyRequestParameters() {
    final RequestParameters request = RequestParameters.build(null, null, null);
    assertNull(request.get(RequestParameters.FROM));
    assertNull(request.get(RequestParameters.TO));
    assertNull(request.get(RequestParameters.LIMIT));
    assertNotNull(request.keySet());
    assertTrue(request.size() == 0);
  }

  @Test
  public void buildBasicRequestParameters() {
    final RequestParameters request = RequestParameters.build(new Date(), new Date(), new Integer(SentiloConstants.NUM_MAXIM_ELEMENTS));
    assertNotNull(request.get(RequestParameters.FROM));
    assertNotNull(request.get(RequestParameters.TO));
    assertNotNull(request.get(RequestParameters.LIMIT));
    assertNotNull(request.keySet());
    assertTrue(request.size() == 3);
  }

  @Test
  public void buildRequestParameters() {
    final RequestParameters request = RequestParameters.build(new Date(), new Date(), new Integer(SentiloConstants.NUM_MAXIM_ELEMENTS));
    request.put("a", SentiloConstants.DEFAULT_NUM_ELEMENTS);
    assertNotNull(request.get(RequestParameters.FROM));
    assertNotNull(request.get(RequestParameters.TO));
    assertNotNull(request.get(RequestParameters.LIMIT));
    assertNotNull(request.get("a"));
    assertNotNull(request.keySet());
    assertTrue(request.size() == 4);
  }

  @Test
  public void buildDeleteRequestParameters() {
    final RequestParameters request = RequestParameters.buildDelete();
    assertNotNull(request.get(RequestParameters.METHOD));
    assertTrue(request.size() == 1);
    assertEquals(RequestParameters.DELETE, request.get(RequestParameters.METHOD));
  }

}
