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
package org.sentilo.platform.service.notification;

import java.util.HashMap;
import java.util.Map;

import org.sentilo.common.enums.HttpHeader;
import org.sentilo.common.rest.RequestContext;
import org.sentilo.common.rest.impl.RESTClientImpl;
import org.sentilo.platform.common.domain.NotificationParams;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.ratelimiter.service.RateLimiterService;
import org.sentilo.platform.common.service.InternalAlarmService;
import org.sentilo.platform.service.monitor.CounterContext;
import org.sentilo.platform.service.monitor.CounterEvent;
import org.sentilo.platform.service.monitor.RequestType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

@Service
public class NotificationDeliveryServiceImpl implements NotificationDeliveryService {

  private static final Logger LOGGER = LoggerFactory.getLogger(NotificationDeliveryServiceImpl.class);

  @Autowired
  private NotificationRetryRepository repository;

  @Autowired
  private InternalAlarmService internalAlarmService;

  @Autowired
  private ApplicationContext context;

  @Value("${api.retry.notifications:true}")
  private boolean retryNotificationsEnabled = true;

  @Autowired
  @Qualifier("outboundRateLimiting")
  private RateLimiterService rateLimitingService;

  private Map<String, RESTClientImpl> restClients = new HashMap<String, RESTClientImpl>();

  @Override
  public void pushNotification(final String message, final NotificationDeliveryContext notificationContext) {
    pushNotification(new NotificationRetryEvent(message, notificationContext, 0));
  }

  @Override
  public void pushNotification(final NotificationRetryEvent notificationRetryEvent) {
    final NotificationDeliveryContext notificationContext = notificationRetryEvent.getNotificationDeliveryContext();
    boolean successProcess = false;

    try {
      // TODO: What happens with relays and with delay between them when quota is exceeded? Should
      // apply same values?
      successProcess = isAllowed(notificationContext.getEntity()) && sent(notificationRetryEvent);
    } catch (final Exception e) {
      successProcess = false;
    } finally {
      if (!successProcess && retryNotificationsEnabled) {
        saveForFurtherRetryAttempt(notificationRetryEvent);
      }
      QuotaContextHolder.clearContext();
    }
  }

  public boolean isAllowed(final String entity) {
    final boolean allowed = rateLimitingService.allow(entity);
    if (!allowed) {
      final QuotaContext qc = QuotaContextHolder.getContext(QuotaContext.Type.ENTITY);
      LOGGER.warn("Entity [{}] has exceeded its outbound quota limit [{} items/hour] !", entity, qc.getLimit());
      internalAlarmService.publishOutboundRateLimiterAlarm(entity, qc);
    }

    return allowed;
  }

  protected boolean sent(final NotificationRetryEvent notificationRetryEvent) {
    final NotificationDeliveryContext notificationContext = notificationRetryEvent.getNotificationDeliveryContext();
    final NotificationParams notificationParams = notificationContext.getNotificationParams();
    boolean sentOk = true;

    try {
      final RESTClientImpl restClient = getRestClient(notificationContext.getEntity());
      LOGGER.info("Push notification to endpoint {} : {}", notificationParams.getEndpoint(), notificationRetryEvent.getMessage());
      final RequestContext rc = new RequestContext("", notificationRetryEvent.getMessage());
      rc.setHost(notificationParams.getEndpoint());
      rc.setSecretKey(notificationParams.getSecretCallbackKey());

      // Add additional headers
      addAdditionalHeaders(rc);

      restClient.post(rc);

      publishPushCounterEvent(notificationContext);
    } catch (final Exception e) {
      sentOk = false;
      LOGGER.warn("Error sending push notification {} to {}. Number of retries: {} ", notificationRetryEvent.getMessage(),
          notificationContext.getNotificationParams().getEndpoint(), notificationRetryEvent.getRetryCount());
    }

    return sentOk;
  }

  private void addAdditionalHeaders(final RequestContext rc) {
    // Add Rate Limiter headers
    if (QuotaContextHolder.hasEntityContext()) {
      final QuotaContext qc = QuotaContextHolder.getContext(QuotaContext.Type.ENTITY);
      rc.addHeader(HttpHeader.X_RL_OUTPUT_LIMIT.name(), Long.toString(qc.getLimit()));
      rc.addHeader(HttpHeader.X_RL_OUTPUT_REMAINING.name(), Long.toString(qc.getRemaining()));
      rc.addHeader(HttpHeader.X_RL_OUTPUT_RESET.name(), Long.toString(qc.getMinutesToReset()));
    }
  }

  /**
   * If {@link NotificationParams#getMaxRetries()} is greater than
   * {@link NotificationRetryEvent#getRetryCount()}, saves the message in a queue for further retry
   * 
   * @param notificationRetryEvent
   */
  protected boolean saveForFurtherRetryAttempt(final NotificationRetryEvent notificationRetryEvent) {
    final long maxRetries = notificationRetryEvent.getNotificationDeliveryContext().getNotificationParams().getMaxRetries();
    final long retryCount = notificationRetryEvent.getRetryCount();

    final boolean retry = retryCount < maxRetries ? true : false;

    if (retry) {
      repository.save(notificationRetryEvent);
      LOGGER.info("Message {} enqueued to be processed further", notificationRetryEvent.getMessage());
    } else {
      final String endpoint = notificationRetryEvent.getNotificationDeliveryContext().getNotificationParams().getEndpoint();
      LOGGER.warn("Push message {} to {} is rejected because the number of maximum retries ({}) has been achieved.",
          notificationRetryEvent.getMessage(), endpoint, maxRetries);
    }

    return retry;
  }

  protected void publishPushCounterEvent(final NotificationDeliveryContext notificationContext) {
    final CounterContext counterContext =
        new CounterContext(notificationContext.getEntity(), notificationContext.getTenant(), RequestType.PUSH, notificationContext.getEventType(), 1);
    context.publishEvent(new CounterEvent(counterContext));
  }

  /**
   * Each listener has its own RestClient to push notifications (each containing up to 400
   * connections) .
   *
   * @param sourceListener Listener unique identifier
   * @return
   * @see RESTClientImpl#afterPropertiesSet()
   *
   */
  private RESTClientImpl getRestClient(final String sourceListener) {
    RESTClientImpl restClient = restClients.get(sourceListener);
    if (restClient == null) {
      restClient = context.getBean("pushRestClient", RESTClientImpl.class);
      restClients.put(sourceListener, restClient);
    }

    return restClient;
  }

}
