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
package org.sentilo.agent.common.service.impl;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.sentilo.agent.common.listener.AbstractMessageListenerImpl;
import org.sentilo.agent.common.service.AsyncPendingEventService;
import org.sentilo.common.domain.EventMessage;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.Topic;
import org.springframework.stereotype.Service;

@Service
public class AsyncPendingEventServiceImpl implements AsyncPendingEventService {

  private Map<Topic, MessageListener> topicMapping = new ConcurrentHashMap<Topic, MessageListener>();

  @Override
  public void process(final EventMessage event) {
    // event must be forwarded to each listener subscribed to its topic
    final String channel = event.getTopic();

    for (final Topic topic : topicMapping.keySet()) {
      if (matches(topic, channel)) {
        processEvent(topicMapping.get(topic), event);
      }
    }
  }

  public void addMessageListener(final MessageListener listener, final Topic topic) {
    topicMapping.put(topic, listener);
  }

  private boolean matches(final Topic topic, final String channel) {
    final Pattern r = Pattern.compile(buildPattern(topic));
    final Matcher m = r.matcher(channel);

    return m.matches();
  }

  private String buildPattern(final Topic topic) {
    if (topic instanceof ChannelTopic) {
      return topic.getTopic();
    } else {
      // Convert topic pattern in a valid regular expression, i.e., replace the last * character
      // with .*
      return topic.getTopic().replaceAll("\\*", ".*");
    }
  }

  /**
   * Retry the original action with the pending event <code>event</code>.
   *
   * @param messageListener
   * @param event
   */
  protected void processEvent(final MessageListener messageListener, final EventMessage event) {
    ((AbstractMessageListenerImpl) messageListener).doWithMessage(event);
  }

}
