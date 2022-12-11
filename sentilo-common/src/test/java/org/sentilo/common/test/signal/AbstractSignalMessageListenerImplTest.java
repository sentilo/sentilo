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
package org.sentilo.common.test.signal;

import static org.mockito.Mockito.when;

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;
import org.sentilo.common.converter.DefaultStringMessageConverter;
import org.sentilo.common.domain.SignalMessage;
import org.sentilo.common.enums.SignalType;
import org.sentilo.common.signal.AbstractSignalMessageListenerImpl;
import org.springframework.data.redis.connection.Message;

public class AbstractSignalMessageListenerImplTest {

  private AtomicBoolean flag = new AtomicBoolean(false);

  private AbstractSignalMessageListenerImpl listener = new MockSignalMessageListenerImpl();

  @Test
  public void onMessage() {
    final String sChannel = "/data/provider1/sensor1";
    final String sPattern = "/data";
    final SignalMessage signal = new SignalMessage(System.currentTimeMillis(), SignalType.DELETE_ALERTS, "mockAgent");
    final String sBody = new DefaultStringMessageConverter().marshal(signal);
    final byte[] body = sBody.getBytes();
    final byte[] channel = sChannel.getBytes();
    final byte[] pattern = sPattern.getBytes();
    final Message message = Mockito.mock(Message.class);
    when(message.getBody()).thenReturn(body);
    when(message.getChannel()).thenReturn(channel);

    listener.onMessage(message, pattern);

    Assert.assertTrue(flag.get());
  }

  class MockSignalMessageListenerImpl extends AbstractSignalMessageListenerImpl {

    @Override
    public void doWithMessage(final SignalMessage message) {
      if (message.getSignal().equals(SignalType.DELETE_ALERTS) && message.getSender().equals("mockAgent")) {
        flag.set(true);
      }
    }

  }

}
