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
package org.sentilo.agent.historian.utils;

import static org.sentilo.agent.historian.utils.OpenTSDBValueConverter.replaceIllegalCharacters;

import java.text.ParseException;
import java.util.LinkedHashMap;
import java.util.Map;

import org.sentilo.agent.historian.domain.OpenTSDBDataPoint;
import org.sentilo.agent.historian.domain.OpenTSDBDataPoint.Tags;
import org.sentilo.common.domain.EventMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

public class OpenTSDBDataPointMarshaller {

  private static final Logger LOGGER = LoggerFactory.getLogger(OpenTSDBDataPointMarshaller.class);

  private static Boolean usePublishedAtTimestamp = true;

  public static OpenTSDBDataPoint unmarshal(final EventMessage event) throws ParseException {
    final OpenTSDBDataPoint dataPoint = new OpenTSDBDataPoint();

    dataPoint.setMetric(OpenTSDBValueConverter.createMetricName(event));
    dataPoint.setValue(OpenTSDBValueConverter.getSimpleValue(event.getMessage()));
    if (usePublishedAtTimestamp && event.getPublishedAt() != null) {
      dataPoint.setTimestamp(event.getPublishedAt());
    } else {
      dataPoint.setTimestamp(event.getTime());
    }

    dataPoint.setTags(createTags(event));

    return dataPoint;

  }

  private static Map<String, String> createTags(final EventMessage event) {
    final Map<String, String> tags = new LinkedHashMap<String, String>();
    putTag(tags, Tags.type.name(), replaceIllegalCharacters(event.getType()));
    putTag(tags, Tags.sensor.name(), replaceIllegalCharacters(event.getSensor()));
    putTag(tags, Tags.provider.name(), replaceIllegalCharacters(event.getProvider()));
    putTag(tags, Tags.component.name(), replaceIllegalCharacters(event.getComponent()));
    putTag(tags, Tags.alertType.name(), replaceIllegalCharacters(event.getAlertType()));
    putTag(tags, Tags.sensorType.name(), replaceIllegalCharacters(event.getSensorType()));
    putTag(tags, Tags.publisher.name(), replaceIllegalCharacters(event.getPublisher()));
    putTag(tags, Tags.tenant.name(), replaceIllegalCharacters(event.getTenant()));
    putTag(tags, Tags.publisherTenant.name(), replaceIllegalCharacters(event.getPublisherTenant()));

    return tags;
  }

  private static void putTag(final Map<String, String> tags, final String tagName, final String tagValue) {
    if (StringUtils.hasText(tagValue)) {
      tags.put(tagName, tagValue);
    }
  }

  public static Boolean getUsePublishedAtTimestamp() {
    return usePublishedAtTimestamp;
  }

  public static void setUsePublishedAtTimestamp(final Boolean usePublishedAtTimestamp) {
    LOGGER.info("SETTING usePublishedAtTimestamp to {}", usePublishedAtTimestamp);
    OpenTSDBDataPointMarshaller.usePublishedAtTimestamp = usePublishedAtTimestamp;
  }

}
