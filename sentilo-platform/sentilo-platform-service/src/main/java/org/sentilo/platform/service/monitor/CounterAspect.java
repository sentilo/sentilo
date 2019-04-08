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
package org.sentilo.platform.service.monitor;

import java.util.Collection;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.sentilo.common.domain.PlatformInputMessage;
import org.sentilo.common.enums.EventType;
import org.sentilo.platform.common.domain.DataInputMessage;
import org.sentilo.platform.common.exception.EventRejectedException;
import org.sentilo.platform.common.security.RequesterContext;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;

@Component
@Aspect
public class CounterAspect implements Ordered, ApplicationContextAware {

  private final int order = 1;
  private ApplicationContext context;

  @Pointcut("execution(* org.sentilo.platform.service.impl.*.set*(..)) && args(inputMessage) && @annotation(metric)")
  public void setInputMessage(final PlatformInputMessage inputMessage, final Metric metric) {
    // Do nothing. Pointcut definition
  }

  @Pointcut("execution(* org.sentilo.platform.service.impl.*.getLast*(..)) && args(inputMessage) && @annotation(metric)")
  public void getLastMessage(final PlatformInputMessage inputMessage, final Metric metric) {
    // Do nothing. Pointcut definition
  }

  // @Before("setInputMessage(inputMessage, metric)")
  public void setAdvice(final JoinPoint jp, final PlatformInputMessage inputMessage, final Metric metric) {
    switch (metric.eventType()) {
      case DATA:
        publishCounterEvent(metric.requestType(), metric.eventType(), ((DataInputMessage) inputMessage).getObservations().size());
        break;
      default:
        publishCounterEvent(metric.requestType(), metric.eventType(), 1);
        break;
    }

    return;
  }

  @Around("setInputMessage(inputMessage, metric)")
  public void setAroundAdvice(final ProceedingJoinPoint jp, final PlatformInputMessage inputMessage, final Metric metric) throws Throwable {
    int totalResourcesRejected = 0;
    try {
      jp.proceed();
    } catch (final EventRejectedException ere) {
      totalResourcesRejected = ere.getRejectedEventsCount();
      throw ere;
    } finally {
      publishCounterEvent(inputMessage, metric, totalResourcesRejected);
    }
  }

  @AfterReturning(pointcut = "getLastMessage(inputMessage, metric)", returning = "lastEvents")
  public void getLastAdvice(final JoinPoint jp, final PlatformInputMessage inputMessage, final Metric metric, final Object lastEvents) {
    if (lastEvents instanceof Collection<?>) {
      publishCounterEvent(metric.requestType(), metric.eventType(), ((Collection<?>) lastEvents).size());
    }

    return;
  }

  protected void publishCounterEvent(final PlatformInputMessage inputMessage, final Metric metric, final int totalResourcesRejected) {
    switch (metric.eventType()) {
      case DATA:
        final int totalDataResources = ((DataInputMessage) inputMessage).getObservations().size() - totalResourcesRejected;
        publishCounterEvent(metric.requestType(), metric.eventType(), totalDataResources);
        break;
      default:
        final int totalResources = 1 - totalResourcesRejected;
        publishCounterEvent(metric.requestType(), metric.eventType(), totalResources);
        break;
    }
  }

  protected void publishCounterEvent(final RequestType requestType, final EventType dataType, final int total) {
    final RequesterContext requesterContext = RequesterContextHolder.getContext();
    final CounterContext counterContext =
        new CounterContext(requesterContext.getEntityId(), requesterContext.getTenantId(), requestType, dataType, total);
    context.publishEvent(new CounterEvent(counterContext));
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.springframework.context.ApplicationContextAware#setApplicationContext(org.springframework
   * .context.ApplicationContext)
   */
  @Override
  public void setApplicationContext(final ApplicationContext applicationContext) {
    context = applicationContext;
  }

  @Override
  public int getOrder() {
    return order;
  }
}
