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
package org.sentilo.web.catalog.test.format.misc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.config.SentiloArtifactConfigService;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.web.catalog.domain.Application;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.domain.Sensor.DataType;
import org.sentilo.web.catalog.format.misc.SensorValueFormatter;
import org.sentilo.web.catalog.service.ApplicationService;
import org.sentilo.web.catalog.utils.Constants;

public class SensorValueFormatterTest {

  @Mock
  private SentiloArtifactConfigService configService;

  @Mock
  private ApplicationService applicationService;

  @Mock
  private Sensor sensor;

  @Mock
  private Observation observation;

  @Mock
  private Application application;

  @InjectMocks
  private SensorValueFormatter valueFormatter;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void formatNoLinkValue() {
    final String obsValue = "Some text here";
    when(sensor.getDataType()).thenReturn(DataType.TEXT);
    when(observation.getValue()).thenReturn(obsValue);

    final String formattedValue = valueFormatter.formatValue(sensor, observation);

    assertEquals(obsValue, formattedValue);
  }

  @Test
  public void formatNoS3LinkValue() {
    final String noS3Link = "http://www.acme.io/resource";

    when(sensor.getDataType()).thenReturn(DataType.LINK);
    when(observation.getValue()).thenReturn(noS3Link);

    final String formattedValue = valueFormatter.formatValue(sensor, observation);

    assertEquals(noS3Link, formattedValue);
  }

  @Test
  public void formatWrongS3LinkValue() {
    final String wrongS3Link = "http://127.0.0.1:8000/s3.sentilo-catalog.bucket/my-path/mockFile.mp3";
    final String s3Endpoints = "127.0.0.1:8000,127.0.0.1:8001";

    when(sensor.getDataType()).thenReturn(DataType.AUDIO_LINK);
    when(observation.getValue()).thenReturn(wrongS3Link);
    when(configService.getConfigValue("sentilo.s3.endpoints")).thenReturn(s3Endpoints);

    final String formattedValue = valueFormatter.formatValue(sensor, observation);

    assertEquals(wrongS3Link, formattedValue);
  }

  @Test
  public void formatS3LinkValue() {
    final String audioLink = "http://127.0.0.1:8000/s3.sentilo-catalog.bucket/audio.mp3";
    final String s3Endpoints = "127.0.0.1:8000,127.0.0.1:8001";

    when(sensor.getDataType()).thenReturn(DataType.AUDIO_LINK);
    when(observation.getValue()).thenReturn(audioLink);
    when(configService.getConfigValue("sentilo.s3.endpoints")).thenReturn(s3Endpoints);
    when(configService.getConfigValue("sentilo.s3.signing.region", Constants.S3_SIGNING_REGION)).thenReturn(Constants.S3_SIGNING_REGION);
    when(configService.getConfigValue("sentilo.s3.url.ttl", String.valueOf(Constants.S3_LINK_DEFAULT_TTL)))
        .thenReturn(String.valueOf(Constants.S3_LINK_DEFAULT_TTL));
    when(configService.getConfigValue(Constants.CATALOG_MASTER_APP_ID)).thenReturn(SentiloConstants.DEFAULT_CATALOG_ID);
    when(applicationService.find(any(Application.class))).thenReturn(application);
    when(application.getToken()).thenReturn("mockToken");

    final String formattedValue = valueFormatter.formatValue(sensor, observation);

    assertNotEquals(audioLink, formattedValue);
    assertTrue(formattedValue.indexOf("X-Amz-Credential=sentilo-catalog") > 0);

  }

}
