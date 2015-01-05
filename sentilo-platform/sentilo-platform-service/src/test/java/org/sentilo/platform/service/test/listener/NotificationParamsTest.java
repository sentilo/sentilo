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
package org.sentilo.platform.service.test.listener;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.service.listener.NotificationParams;

public class NotificationParamsTest {

  static final String SECRET = "123456789";
  static final String ENDPOINT = "http://127.0.0.1/endpoint";

  @Test
  public void buildFromParamsChain() {
    final String chain = ENDPOINT + SentiloConstants.SENTILO_INTERNAL_TOKEN + SECRET;
    final NotificationParams notificationParams = new NotificationParams(chain);

    Assert.assertEquals(ENDPOINT, notificationParams.getEndpoint());
    Assert.assertEquals(SECRET, notificationParams.getSecretCallbackKey());
  }

  @Test
  public void buildFromParamsChainWithoutSecret() {
    final String chain = ENDPOINT;
    final NotificationParams notificationParams = new NotificationParams(chain);

    Assert.assertEquals(ENDPOINT, notificationParams.getEndpoint());
    Assert.assertNull(notificationParams.getSecretCallbackKey());
  }

  @Test
  public void buildFromParams() {
    final NotificationParams notificationParams = new NotificationParams(ENDPOINT, SECRET);

    Assert.assertEquals(ENDPOINT, notificationParams.getEndpoint());
    Assert.assertEquals(SECRET, notificationParams.getSecretCallbackKey());
  }

  @Test
  public void checkToString() {
    final NotificationParams notificationParams = new NotificationParams(ENDPOINT, SECRET);
    final NotificationParams notificationParams2 = new NotificationParams(ENDPOINT);

    Assert.assertTrue(notificationParams.toString().contains("secretCallbackKey"));
    Assert.assertFalse(notificationParams2.toString().contains("secretCallbackKey"));

  }
}
