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

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.rest.hmac.HMACBuilder;

public class HMACBuilderTest {

  @Test
  public void buildHmacHeader() throws Exception {
    final String hmacExpected = "j1OQ+fU667GQoHYHWzLBpigRjLJmRvYn53KHZhApTbrcphYWBlRPSBHkntODuqsqx11Vj8rsc7DDziiutTq/5g==";

    final String secret = "123456789";
    final String currentDate = "10/06/2014T15:27:22";
    final String endpoint = "http://l27.0.0.1/endpoint";
    final String body = "{\"field1\":\"value1\",\"field2\":\"value2\"}";

    final String hmac = HMACBuilder.buildHeader(body, endpoint, secret, currentDate);
    Assert.assertEquals(hmacExpected, hmac);
  }

  @Test
  public void buildWrongHmacHeader() throws Exception {
    final String hmacExpected = "j1OQ+fU667GQoHYHWzLBpigRjLJmRvYn53KHZhApTbrcphYWBlRPSBHkntODuqsqx11Vj8rsc7DDziiutTq/5g==";

    final String secret = "123456789";
    final String currentDate = "10/06/2014T15:27:24";
    final String endpoint = "http://l27.0.0.1/endpoint";
    final String body = "{\"field1\":\"value1\",\"field2\":\"value2\"}";

    final String hmac = HMACBuilder.buildHeader(body, endpoint, secret, currentDate);

    Assert.assertNotEquals(hmacExpected, hmac);
  }

  @Test
  public void checkHeader() throws Exception {
    final String hmacHeaderValue = "j1OQ+fU667GQoHYHWzLBpigRjLJmRvYn53KHZhApTbrcphYWBlRPSBHkntODuqsqx11Vj8rsc7DDziiutTq/5g==";

    final String secret = "123456789";
    final String currentDate = "10/06/2014T15:27:22";
    final String endpoint = "http://l27.0.0.1/endpoint";
    final String body = "{\"field1\":\"value1\",\"field2\":\"value2\"}";

    Assert.assertTrue(HMACBuilder.checkHeader(hmacHeaderValue, body, endpoint, secret, currentDate));

  }
}
