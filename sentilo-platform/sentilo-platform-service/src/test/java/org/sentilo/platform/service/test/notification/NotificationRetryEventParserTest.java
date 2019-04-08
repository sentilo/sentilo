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
package org.sentilo.platform.service.test.notification;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.service.notification.NotificationDeliveryContext;
import org.sentilo.platform.service.notification.NotificationRetryEvent;
import org.sentilo.platform.service.notification.NotificationRetryEventConverter;

public class NotificationRetryEventParserTest {

  private NotificationRetryEventConverter parser = new NotificationRetryEventConverter();

  @Test
  public void marshall() {
    final NotificationParams params = new NotificationParams("http://dev.sentilo.io", "ABCDEF12345", 10, 2);
    final NotificationDeliveryContext context = new NotificationDeliveryContext(params, "mockEntity", null, EventType.DATA);
    final String message =
        "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\",\"type\":\"data\"}";
    final NotificationRetryEvent event = new NotificationRetryEvent(message, context, 0);

    final String expected =
        "{\"message\":\"{\\\"message\\\":\\\"stop start sensors\\\",\\\"timestamp\\\":\\\"21/03/2013T14:25:39\\\",\\\"topic\\\":\\\"/data/provider23\\\",\\\"type\\\":\\\"data\\\"}\",\"notificationDeliveryContext\":{\"notificationParams\":{\"endpoint\":\"http://dev.sentilo.io\",\"secretCallbackKey\":\"ABCDEF12345\",\"maxRetries\":10,\"retryDelay\":2},\"entity\":\"mockEntity\",\"eventType\":\"DATA\"},\"retryCount\":0}";

    final String json = parser.marshall(event);

    Assert.assertEquals(expected, json);
  }

  @Test
  public void unmarshall() {
    final String json =
        "{\"message\":\"{\\\"message\\\":\\\"stop start sensors\\\",\\\"timestamp\\\":\\\"21/03/2013T14:25:39\\\",\\\"topic\\\":\\\"/data/provider23\\\",\\\"type\\\":\\\"data\\\"}\",\"notificationDeliveryContext\":{\"notificationParams\":{\"endpoint\":\"http://dev.sentilo.io\",\"secretCallbackKey\":\"ABCDEF12345\",\"maxRetries\":10,\"retryDelay\":2},\"entity\":\"mockEntity\",\"eventType\":\"DATA\"},\"retryCount\":0}";

    final NotificationRetryEvent event = parser.unmarshall(json);

    final String expectedMessage =
        "{\"message\":\"stop start sensors\",\"timestamp\":\"21/03/2013T14:25:39\",\"topic\":\"/data/provider23\",\"type\":\"data\"}";
    Assert.assertEquals("http://dev.sentilo.io", event.getNotificationDeliveryContext().getNotificationParams().getEndpoint());
    Assert.assertEquals("ABCDEF12345", event.getNotificationDeliveryContext().getNotificationParams().getSecretCallbackKey());
    Assert.assertEquals(10, event.getNotificationDeliveryContext().getNotificationParams().getMaxRetries());
    Assert.assertEquals(2, event.getNotificationDeliveryContext().getNotificationParams().getRetryDelay());
    Assert.assertEquals("mockEntity", event.getNotificationDeliveryContext().getEntity());
    Assert.assertEquals(EventType.DATA, event.getNotificationDeliveryContext().getEventType());
    Assert.assertNull(event.getNotificationDeliveryContext().getTenant());
    Assert.assertEquals(0, event.getRetryCount());
    Assert.assertEquals(expectedMessage, event.getMessage());

  }
}
