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
package org.sentilo.web.catalog.aop.aspect;

import java.util.List;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.sentilo.common.domain.OrderMessage;
import org.sentilo.platform.client.core.domain.AlarmMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Activity;
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

  private final int order = 1;

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastObservations(..))", returning = "observations")
  public
      Object translateObservationsTimestamp(final JoinPoint jp, final List<Observation> observations) throws Throwable {
    if (localDateFormat != null && !CollectionUtils.isEmpty(observations)) {
      for (final Observation observation : observations) {
        observation.setTimestamp(translateTimestampIntoLocalTimeZone(observation.getTimestamp()));
      }
    }
    return observations;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastObservation(..))", returning = "observation")
  public Object translateObservationTimestamp(final JoinPoint jp, final Observation observation) throws Throwable {
    if (localDateFormat != null && observation != null) {
      observation.setTimestamp(translateTimestampIntoLocalTimeZone(observation.getTimestamp()));
    }
    return observation;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastAlarmsMessages(..))", returning = "alarms")
  public Object translateAlarmsTimestamp(final JoinPoint jp, final List<AlarmMessage> alarms) throws Throwable {
    if (localDateFormat != null && !CollectionUtils.isEmpty(alarms)) {
      for (final AlarmMessage alarmMessage : alarms) {
        alarmMessage.setTimestamp(translateTimestampIntoLocalTimeZone(alarmMessage.getTimestamp()));
      }
    }
    return alarms;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.SensorServiceImpl.getLastOrderMessages(..))", returning = "orders")
  public Object translateOrdersTimestamp(final JoinPoint jp, final List<OrderMessage> orders) throws Throwable {
    if (localDateFormat != null && !CollectionUtils.isEmpty(orders)) {
      for (final OrderMessage orderMessage : orders) {
        orderMessage.setTimestamp(translateTimestampIntoLocalTimeZone(orderMessage.getTimestamp()));
      }
    }
    return orders;
  }

  @AfterReturning(pointcut = "execution(* org.sentilo.web.catalog.service.impl.ActivityServiceImpl.getLastActivityLogs(..))", returning = "lastActivityLogs")
  public
      Object translateActivityTimestamp(final JoinPoint jp, final List<Activity> lastActivityLogs) throws Throwable {
    if (localDateFormat != null && !CollectionUtils.isEmpty(lastActivityLogs)) {
      for (final Activity activityLog : lastActivityLogs) {
        activityLog.setTimestampToString(translateTimestampIntoLocalTimeZone(activityLog.getTimestamp()));
      }
    }
    return lastActivityLogs;
  }

  @Override
  public int getOrder() {
    return order;
  }

  private String translateTimestampIntoLocalTimeZone(final String utcTimestamp) {
    return localDateFormat.parseUtcTimeToLocalTime(utcTimestamp);

  }

  private String translateTimestampIntoLocalTimeZone(final Long timestamp) {
    return localDateFormat.printAsLocalTime(timestamp);
  }

}
