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
package org.sentilo.web.catalog.controller;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;

import org.sentilo.web.catalog.domain.Activity;
import org.sentilo.web.catalog.domain.Performance;
import org.sentilo.web.catalog.domain.Statistics;
import org.sentilo.web.catalog.dto.LastEventsDTO;
import org.sentilo.web.catalog.dto.StatsDTO;
import org.sentilo.web.catalog.format.datetime.LocalDateFormatter;
import org.sentilo.web.catalog.service.ActivityService;
import org.sentilo.web.catalog.service.StatsService;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
@RequestMapping("/stats")
public class StatsController extends CatalogBaseController {

  @Autowired
  private StatsService statsService;

  @Autowired
  private ActivityService activityService;

  @Autowired
  private LocalDateFormatter localDateFormatter;

  @ModelAttribute(Constants.MODEL_ACTIVE_MENU)
  public String getActiveMenu() {
    return Constants.MENU_STATS;
  }

  @ModelAttribute(Constants.MODEL_MAX_SYSTEM_DATE_MILLIS)
  public long getMaxSystemStringDate() {
    return CatalogUtils.getMaxSystemTimeMillis();
  }

  @RequestMapping("/")
  public String initStats() {
    return Constants.VIEW_STATS;
  }

  @RequestMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public StatsDTO getCurrentStats() {
    final StatsDTO result = new StatsDTO();
    final Statistics stats = statsService.getCurrentStats();
    final Performance performance = stats.getPerformance() == null ? new Performance() : stats.getPerformance();
    final DecimalFormat formatter = (DecimalFormat) NumberFormat.getInstance(LocaleContextHolder.getLocale());

    result.setTotalDevices(formatter.format(stats.getDevices().getSensors()));
    result.setTotalRouterDevices(formatter.format(stats.getDevices().getRoutersAndGateways()));
    result.setTotalOtherDevices(formatter.format(stats.getDevices().getOthers()));

    result.setTotalEvents(formatter.format(performance.getTotalRequests()));
    result.setTotalObsEvents(formatter.format(performance.getTotalObs()));
    result.setTotalOrderEvents(formatter.format(performance.getTotalOrders()));
    result.setTotalAlarmEvents(formatter.format(performance.getTotalAlarms()));

    result.setEventsPerSecond(formatter.format(performance.getInstantAvg()));
    result.setDailyAverageRate(formatter.format(performance.getMaxDailyAvg()));
    result.setMaxRate(formatter.format(performance.getMaxAvg()));

    result.setTotalActiveAccounts(formatter.format(stats.getAccounts().getUsers()));
    result.setTotalProviderAccounts(formatter.format(stats.getAccounts().getProviders()));
    result.setTotalApplicationAccounts(formatter.format(stats.getAccounts().getApplications()));

    return result;
  }

  @RequestMapping(value = "/activity/json", produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public LastEventsDTO<Activity> getCurrentActivity(@RequestParam(required = false) final Long from, @RequestParam(required = false) final Long to) {
    // To display the last activity logs, elements must be show in a reverse order (from the most
    // ancient to the most recent), but the list returned by the service is immutable so we need
    // build a new list to reverse the order.
    final List<Activity> lastActivityLogs = new ArrayList<Activity>(activityService.getLastActivityLogs(from, to));
    return createLastActivityEventsDTO(lastActivityLogs);
  }

  private LastEventsDTO<Activity> createLastActivityEventsDTO(final List<Activity> lastActivityLogs) {
    final Long to = CollectionUtils.isEmpty(lastActivityLogs) ? System.currentTimeMillis() : lastActivityLogs.get(0).getTimestamp();
    final Long from = CollectionUtils.isEmpty(lastActivityLogs) ? 0 : lastActivityLogs.get(lastActivityLogs.size() - 1).getTimestamp();
    final LastEventsDTO<Activity> lastEvents = new LastEventsDTO<Activity>(lastActivityLogs, from, to, localDateFormatter);
    lastEvents.reverse();
    return lastEvents;
  }
}
