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
package org.sentilo.common.test.domain;

import java.beans.PropertyDescriptor;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.domain.PlatformActivity;
import org.sentilo.common.domain.PlatformPerformance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;

public class PlatformActivityAndPerformanceTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(PlatformActivityAndPerformanceTest.class);
  private final ObjectMapper objectMapper = new ObjectMapper();
  protected static final JsonEncoding DEFAULT_ENCODING = JsonEncoding.UTF8;

  @Test
  public void marsharAndUnMarshallPerformance() throws Exception {
    final PlatformActivity activity = buildActivity(true);
    final PlatformPerformance performance = new PlatformPerformance(activity, 3.45f, 34.32f, 132.87f, System.currentTimeMillis());
    marshallAndUnmarshall(performance);
  }

  @Test
  public void marsharAndUnMarshallActivity() throws Exception {
    final PlatformActivity activity = buildActivity(true);
    marshallAndUnmarshall(activity);
  }

  private void marshallAndUnmarshall(final Object obj) throws Exception {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final JsonGenerator jsonGenerator = objectMapper.getFactory().createGenerator(out, DEFAULT_ENCODING);
    objectMapper.writeValue(jsonGenerator, obj);
    final String json = out.toString();

    LOGGER.info("{} json result: {}", obj.getClass().getSimpleName(), json);

    final Object actual = objectMapper.readValue(json, getJavaType(obj.getClass()));

    assertAreEquals(obj, actual);
  }

  private void assertAreEquals(final Object expected, final Object actual) throws Exception {
    final PropertyDescriptor[] propertyDescriptors = BeanUtils.getPropertyDescriptors(expected.getClass());
    for (final PropertyDescriptor propertyDescriptor : propertyDescriptors) {
      final Method readMethod = propertyDescriptor.getReadMethod();
      Assert.assertEquals(readMethod.invoke(expected, new Object[] {}), readMethod.invoke(actual, new Object[] {}));
    }
  }

  protected JavaType getJavaType(final Class<?> clazz) {
    return objectMapper.getTypeFactory().constructType(clazz);
  }

  private PlatformActivity buildActivity(final boolean isMaster) {
    final PlatformActivity platformActivity = new PlatformActivity(isMaster ? null : "mockTenant", 1447164322214l, isMaster);
    platformActivity.setTotalAlarms(10);
    platformActivity.setTotalGetAlarms(3);
    platformActivity.setTotalPutAlarms(7);
    platformActivity.setTotalOrders(125);
    platformActivity.setTotalGetOrders(25);
    platformActivity.setTotalPutOrders(100);
    platformActivity.setTotalObs(1000);
    platformActivity.setTotalGetObs(300);
    platformActivity.setTotalPutObs(700);
    platformActivity.setTotalRequests(1135);
    platformActivity.setTotalGetRequests(328);
    platformActivity.setTotalPutRequests(807);

    return platformActivity;
  }
}
