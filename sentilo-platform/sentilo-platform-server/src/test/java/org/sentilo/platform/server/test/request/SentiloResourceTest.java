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
package org.sentilo.platform.server.test.request;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.sentilo.platform.server.request.SentiloResource;

public class SentiloResourceTest {

  private SentiloResource resource;
  private String path;

  @Test
  public void pathWith4Tokens() {
    path = "event/provider/sensor/value";
    resource = new SentiloResource(path);

    assertEquals("event", resource.getResourcePart(0));
    assertEquals("provider", resource.getResourcePart(1));
    assertEquals("sensor", resource.getResourcePart(2));
    assertEquals("value", resource.getResourcePart(3));
  }

  @Test
  public void pathWith3Tokens() {
    path = "event/provider/sensor";
    resource = new SentiloResource(path);

    assertEquals("event", resource.getResourcePart(0));
    assertEquals("provider", resource.getResourcePart(1));
    assertEquals("sensor", resource.getResourcePart(2));
    assertNull(resource.getResourcePart(3));
  }

  @Test
  public void pathWith2Tokens() {
    path = "event/provider";
    resource = new SentiloResource(path);

    assertEquals("event", resource.getResourcePart(0));
    assertEquals("provider", resource.getResourcePart(1));
    assertNull(resource.getResourcePart(2));
    assertNull(resource.getResourcePart(3));
  }

  @Test
  public void pathWith1Token() {
    path = "event";
    resource = new SentiloResource(path);
    assertEquals("event", resource.getResourcePart(0));
    assertNull(resource.getResourcePart(1));
    assertNull(resource.getResourcePart(2));
    assertNull(resource.getResourcePart(3));

    assertEquals("path: event", resource.toString());
  }

  @Test
  public void emptyPath() {
    path = "";
    resource = new SentiloResource(path);
    assertNull(resource.getResourcePart(0));
  }
}
