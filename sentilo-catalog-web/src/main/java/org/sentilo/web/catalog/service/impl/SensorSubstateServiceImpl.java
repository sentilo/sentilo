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
package org.sentilo.web.catalog.service.impl;

import static org.springframework.data.mongodb.core.aggregation.Aggregation.match;
import static org.springframework.data.mongodb.core.aggregation.Aggregation.newAggregation;
import static org.springframework.data.mongodb.core.aggregation.Aggregation.project;
import static org.springframework.data.mongodb.core.aggregation.Aggregation.sort;
import static org.springframework.data.mongodb.core.query.Criteria.where;

import java.util.List;
import java.util.Locale;

import org.sentilo.common.enums.SensorState;
import org.sentilo.web.catalog.domain.SensorSubstate;
import org.sentilo.web.catalog.service.SensorSubstateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.aggregation.Fields;
import org.springframework.stereotype.Service;

@Service
public class SensorSubstateServiceImpl extends AbstractBaseServiceImpl implements SensorSubstateService {

  @Autowired
  private MongoOperations mongoOps;

  @Override
  public List<SensorSubstate> findAll() {

    final Locale locale = LocaleContextHolder.getLocale();

    // Translations searchs are by language and by language_country
    final String localeField = "translations." + locale.getLanguage();
    final String localeLongField = "translations." + locale.toString();

    final Aggregation agg = newAggregation(project(Fields.from(Fields.field("code"), Fields.field("defaultDesc", "text"),
        Fields.field("translateLocShort", localeField), Fields.field("translateLocLong", localeLongField))), sort(Sort.Direction.DESC, "code"));

    final AggregationResults<SensorSubstate> results = mongoOps.aggregate(agg, "sensorSubstate", SensorSubstate.class);

    return results.getMappedResults();
  }

  @Override
  public List<SensorSubstate> findAll(final SensorState state) {

    final Locale locale = LocaleContextHolder.getLocale();

    // Translations searchs are by language and by language_country
    final String localeField = "translations." + locale.getLanguage();
    final String localeLongField = "translations." + locale.toString();

    final Aggregation agg =
        newAggregation(project(Fields.from(Fields.field("code"), Fields.field("defaultDesc", "text"), Fields.field("translateLocShort", localeField),
            Fields.field("translateLocLong", localeLongField))), sort(Sort.Direction.DESC, "code"), match(where("state").is(state)));

    final AggregationResults<SensorSubstate> results = mongoOps.aggregate(agg, "sensorSubstate", SensorSubstate.class);

    return results.getMappedResults();
  }

  @Override
  public SensorSubstate find(final String code) {

    final Locale locale = LocaleContextHolder.getLocale();

    // Translations searchs are by language and by language_country
    final String localeField = "translations." + locale.getLanguage();
    final String localeLongField = "translations." + locale.toString();

    final Aggregation agg = newAggregation(

        project(Fields.from(Fields.field("code"), Fields.field("defaultDesc", "text"), Fields.field("translateLocShort", localeField),
            Fields.field("translateLocLong", localeLongField))),
        sort(Sort.Direction.DESC, "code"), match(where("code").is(code)));

    final AggregationResults<SensorSubstate> results = mongoOps.aggregate(agg, "sensorSubstate", SensorSubstate.class);

    return results.getMappedResults().isEmpty() ? new SensorSubstate() : results.getMappedResults().get(0);
  }
}
