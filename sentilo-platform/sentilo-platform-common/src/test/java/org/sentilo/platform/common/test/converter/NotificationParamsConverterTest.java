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
package org.sentilo.platform.common.test.converter;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.converter.StringMessageConverter;
import org.sentilo.platform.common.domain.NotificationParams;

public class NotificationParamsConverterTest {

  private StringMessageConverter converter = new DefaultStringMessageConverter();

  @Test
  public void marshall() {
    final NotificationParams params = new NotificationParams("http://dev.sentilo.io", null, 0, 0);
    final NotificationParams params2 = new NotificationParams("http://dev.sentilo.io", "ABCDEF12345", 10, 2);
    final String expected = "{\"endpoint\":\"http://dev.sentilo.io\"}";
    final String expected2 = "{\"endpoint\":\"http://dev.sentilo.io\",\"secretCallbackKey\":\"ABCDEF12345\",\"maxRetries\":10,\"retryDelay\":2}";

    final String json = converter.marshal(params);
    final String json2 = converter.marshal(params2);

    Assert.assertEquals(expected, json);
    Assert.assertEquals(expected2, json2);
  }

  @Test
  public void unmarshall() {
    final String json = "{\"endpoint\":\"http://dev.sentilo.io\"}";
    final String json2 = "{\"endpoint\":\"http://dev.sentilo.io\",\"secretCallbackKey\":\"ABCDEF12345\",\"maxRetries\":10,\"retryDelay\":2}";

    final NotificationParams params = (NotificationParams) converter.unmarshal(json, NotificationParams.class);
    final NotificationParams params2 = (NotificationParams) converter.unmarshal(json2, NotificationParams.class);

    Assert.assertEquals("http://dev.sentilo.io", params.getEndpoint());
    Assert.assertNull(params.getSecretCallbackKey());
    Assert.assertEquals(0, params.getMaxRetries());
    Assert.assertEquals(0, params.getRetryDelay());

    Assert.assertEquals("http://dev.sentilo.io", params2.getEndpoint());
    Assert.assertEquals("ABCDEF12345", params2.getSecretCallbackKey());
    Assert.assertEquals(10, params2.getMaxRetries());
    Assert.assertEquals(2, params2.getRetryDelay());

  }
}
