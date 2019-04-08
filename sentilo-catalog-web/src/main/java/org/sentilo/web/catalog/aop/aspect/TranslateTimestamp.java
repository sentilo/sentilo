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
package org.sentilo.web.catalog.aop.aspect;

import java.util.List;

import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Activity;
import org.sentilo.web.catalog.domain.SortedEventsList;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * Class that translates platform's timestamps (expressed in UTC timezone) into local time zones.
 * The local timezone is defined as a key (catalog.default.timezone) in a property file
 * (catalog-config.properties). By default, its value is UTC
 */
@Component
@Aspect
public class TranslateTimestamp implements Ordered {

  @Autowired
  private LocalDateFormatter localDateFormat;

  private static final int ORDER = 1;

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastObservations(..))", returning = "observations")
  public Object translateObservationsTimestamp(final SortedEventsList<Observation> observations) {
    if (localDateFormat != null && !observations.isEmpty()) {
      observations.setTo(observations.first().getTime());
      observations.setFrom(observations.last().getTime());
      for (final Observation observation : observations.getEvents()) {
        observation.setTimestamp(translateTimestampIntoLocalTimeZone(observation.getTime()));
      }
    }
    return observations;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastObservation(..))", returning = "observation")
  public Object translateObservationTimestamp(final Observation observation) {
    if (localDateFormat != null && observation != null) {
      observation.setTimestamp(translateTimestampIntoLocalTimeZone(observation.getTime()));
    }
    return observation;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastAlarmsMessages(..))", returning = "alarms")
  public Object translateAlarmsTimestamp(final SortedEventsList<AlarmMessage> alarms) {
    if (localDateFormat != null && !alarms.isEmpty()) {
      alarms.setTo(alarms.first().getTime());
      alarms.setFrom(alarms.last().getTime());
      for (final AlarmMessage alarmMessage : alarms.getEvents()) {
        alarmMessage.setTimestamp(translateTimestampIntoLocalTimeZone(alarmMessage.getTime()));
      }
    }
    return alarms;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastOrderMessages(..))", returning = "orders")
  public Object translateOrdersTimestamp(final SortedEventsList<OrderMessage> orders) {
    if (localDateFormat != null && !orders.isEmpty()) {
      for (final OrderMessage orderMessage : orders.getEvents()) {
        orders.setTo(orders.first().getTime());
        orders.setFrom(orders.last().getTime());
        orderMessage.setTimestamp(translateTimestampIntoLocalTimeZone(orderMessage.getTime()));
      }
    }
    return orders;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.ActivityServiceImpl.getLastActivityLogs(..))", returning = "lastActivityLogs")
  public List<Activity> translateActivityTimestamp(final List<Activity> lastActivityLogs) {
    if (localDateFormat != null && !CollectionUtils.isEmpty(lastActivityLogs)) {
      for (final Activity activityLog : lastActivityLogs) {
        activityLog.setTimestampToString(translateTimestampIntoLocalTimeZone(activityLog.getTimestamp()));
      }
    }
    return lastActivityLogs;
  }

  @Override
  public int getOrder() {
    return ORDER;
  }

  private String translateTimestampIntoLocalTimeZone(final Long timestamp) {
    return localDateFormat.printAsLocalTime(timestamp);
  }

}
