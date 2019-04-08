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
package org.sentilo.agent.location.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.Test;
import org.sentilo.agent.location.parser.CatalogMessageConverter;
import org.sentilo.common.domain.CatalogInputMessage;
import org.sentilo.common.domain.SensorLocationElement;

public class CatalogMessageConverterTest {

  CatalogMessageConverter converter = new CatalogMessageConverter();

  @Test
  public void marshallEmpty() {
    final CatalogInputMessage message = new CatalogInputMessage();
    final String json = converter.marshall(message);
    final String body = "{}";
    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshall() {
    final CatalogInputMessage message = new CatalogInputMessage();
    message.setLocations(getResourcesToUpdate());
    final String json = converter.marshall(message);
    final String body = "{}";
    assertNotNull(json);
    assertNotEquals(body, json);
  }

  private List<SensorLocationElement> getResourcesToUpdate() {
    final List<SensorLocationElement> resources = new ArrayList<SensorLocationElement>();

    final Random randomGenerator = new Random();
    int size = 0;
    do {
      size = randomGenerator.nextInt(30);
    } while (size == 0);

    for (int i = 0; i < size; i++) {
      final SensorLocationElement resource = new SensorLocationElement();
      resource.setProvider("Provider" + i);
      resource.setSensor("Sensor" + i);
      resource.setLocation("32.111 43.1234");
      resource.setFromTsTime(System.currentTimeMillis() + size % (i + 1));

      resources.add(resource);
    }

    return resources;
  }

}
