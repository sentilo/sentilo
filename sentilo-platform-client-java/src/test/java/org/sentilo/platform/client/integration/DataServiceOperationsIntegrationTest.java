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
package org.sentilo.platform.client.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;
import org.sentilo.common.domain.QueryFilterParams;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.platform.client.core.PlatformTemplate;
import org.sentilo.platform.client.core.domain.DataInputMessage;
import org.sentilo.platform.client.core.domain.Observation;
import org.sentilo.platform.client.core.domain.ObservationsOutputMessage;
import org.sentilo.platform.client.core.domain.ProviderObservations;
import org.sentilo.platform.client.core.domain.SensorObservations;
import org.sentilo.platform.client.core.exception.PlatformClientAccessException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.CollectionUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/sentilo-platform-client-integration.xml")
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class DataServiceOperationsIntegrationTest {

  static String PROVIDER_ID = "testApp_provider";
  static String APP_ID = "testApp";
  static String SENSOR1 = "sensor1";
  static String SENSOR2 = "sensor2";

  @Autowired
  protected PlatformTemplate platformTemplate;

  @Test
  public void _01_sendSimpleObservation() throws Exception {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, "23");
    platformTemplate.getDataOps().sendObservations(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _02_sendForbiddenSimpleObservation() throws Exception {
    final DataInputMessage message = new DataInputMessage("testDemo", SENSOR1, "23");
    boolean error = false;
    try {
      platformTemplate.getDataOps().sendObservations(message);
    } catch (final PlatformClientAccessException e) {
      error = true;
    }
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", error);
  }

  @Test
  public void _03_sendSensorObservations() throws Exception {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1);
    final SensorObservations sensorObservations = new SensorObservations(SENSOR1);
    sensorObservations.setObservations(getObservations());
    message.setSensorObservations(sensorObservations);
    platformTemplate.getDataOps().sendObservations(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _04_sendProviderObservations() throws Exception {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID);
    final ProviderObservations providerObservations = new ProviderObservations();
    providerObservations.setSensorsObservations(getSensorObservations());
    message.setProviderObservations(providerObservations);
    platformTemplate.getDataOps().sendObservations(message);
    assertTrue("No se ha realizado correctamente la llamada a la plataforma", true);
  }

  @Test
  public void _05_getLastObservationFromSensor() throws Exception {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getObservations()));
    assertTrue(response.getObservations().size() == 1);
  }

  @Test
  public void _06_getLastObservationsFromSensor() throws Exception {
    final int num_observations = 3;
    final QueryFilterParams filterParams = new QueryFilterParams(3);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, filterParams);

    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getObservations()));
    assertEquals(num_observations, response.getObservations().size());
  }

  @Test
  public void _07_getEmptyLastObservationsFromSensorInPeriod() throws Exception {
    final QueryFilterParams filterParams =
        new QueryFilterParams(new Date(System.currentTimeMillis() + 1000000), new Date(System.currentTimeMillis() + 2000000));
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, filterParams);

    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && CollectionUtils.isEmpty(response.getObservations()));
  }

  @Test
  public void _08_getLastObservationFromSensorInPeriod() throws Exception {
    final QueryFilterParams filterParams = new QueryFilterParams(new Date(System.currentTimeMillis() - 6000000), new Date());
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, filterParams);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getObservations()));
    assertEquals(1, response.getObservations().size());
  }

  @Test
  public void _09_getLastObservationsFromSensorInPeriod() throws Exception {
    final int num_observations = 3;
    final QueryFilterParams filterParams = new QueryFilterParams(new Date(System.currentTimeMillis() - 6000000), new Date(), num_observations);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, filterParams);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getObservations()));
    assertEquals(num_observations, response.getObservations().size());
  }

  @Test
  public void _10_getLastObservationFromProvider() throws Exception {
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getSensors()));
    // La lista contiene almenos un registro, el del sensor utilizado en este test
    assertTrue(response.getSensors().size() >= 1);
    // Y para cada sensor tendremos sólo un registro
    assertTrue(countObservations(response.getSensors()) == response.getSensors().size());
  }

  @Test
  public void _11_getLastObservationsFromProvider() throws Exception {
    final int num_observations = 3;
    final QueryFilterParams filterParams = new QueryFilterParams(num_observations);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, filterParams);

    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getSensors()));
    // La lista contiene almenos dos registros, uno para cada sensor del proveedor que hemos
    // utilizado en el test
    assertTrue(response.getSensors().size() >= 2);
    // Y para cada sensor tenemos almenos 2 observaciones, las utilizadas en este test --> un minimo
    // de 4 observaciones en total
    assertTrue(countObservations(response.getSensors()) >= 4);
  }

  @Test
  public void _12_getEmptyLastObservationsFromProviderInPeriod() throws Exception {
    final QueryFilterParams filterParams =
        new QueryFilterParams(new Date(System.currentTimeMillis() + 1000000), new Date(System.currentTimeMillis() + 2000000));
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, filterParams);

    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && CollectionUtils.isEmpty(response.getSensors()));
  }

  @Test
  public void _13_getLastObservationFromProviderInPeriod() throws Exception {
    final QueryFilterParams filterParams = new QueryFilterParams(new Date(System.currentTimeMillis() - 60000000), new Date());
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, filterParams);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getSensors()));
    // La lista contiene almenos dos registros, uno para cada sensor del proveedor que hemos
    // utilizado en el test
    assertTrue(response.getSensors().size() >= 2);
    // Y para cada sensor tendremos una observacion
    assertTrue(countObservations(response.getSensors()) == response.getSensors().size());
  }

  @Test
  public void _14_getLastObservationsFromProviderInPeriod() throws Exception {
    final int num_observations = 3;
    final QueryFilterParams filterParams = new QueryFilterParams(new Date(System.currentTimeMillis() - 6000000), new Date(), num_observations);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, filterParams);
    final ObservationsOutputMessage response = platformTemplate.getDataOps().getLastObservations(message);
    assertTrue(response != null && !CollectionUtils.isEmpty(response.getSensors()));
    // La lista contiene almenos dos registros, uno para cada sensor del proveedor que hemos
    // utilizado en el test
    assertTrue(response.getSensors().size() >= 2);
    // Y para cada sensor tenemos almenos 2 observaciones que entran dentro del periodo indicado,
    // las utilizadas en este test --> un minimo de 4 observaciones en total
    assertTrue(countObservations(response.getSensors()) > 4);
  }

  @Test
  public void _15_removeLastObservationsFromSensor() throws Exception {
    final QueryFilterParams filterParams = new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, SENSOR1, filterParams);
    final int countBeforeRemove = platformTemplate.getDataOps().getLastObservations(message).getObservations().size();

    platformTemplate.getDataOps().removeLastObservations(message);

    final int countAfterRemove = platformTemplate.getDataOps().getLastObservations(message).getObservations().size();

    assertTrue(countAfterRemove == SentiloConstants.NUM_MAXIM_ELEMENTS || countAfterRemove == countBeforeRemove - 1);
  }

  @Test
  public void _16_removeLastObservationsFromProvider() throws Exception {

    final QueryFilterParams filterParams = new QueryFilterParams(SentiloConstants.NUM_MAXIM_ELEMENTS);
    final DataInputMessage message = new DataInputMessage(PROVIDER_ID, filterParams);

    final int countBeforeRemove = countObservations(platformTemplate.getDataOps().getLastObservations(message).getSensors());

    platformTemplate.getDataOps().removeLastObservations(message);

    final int countAfterRemove = countObservations(platformTemplate.getDataOps().getLastObservations(message).getSensors());

    assertTrue(countAfterRemove <= countBeforeRemove);
  }

  private int countObservations(final List<SensorObservations> sensors) {
    int count = 0;

    for (final SensorObservations sensorObs : sensors) {
      count += sensorObs.getObservations().size();
    }

    return count;
  }

  private List<Observation> getObservations() {
    final List<Observation> observations = new ArrayList<Observation>();
    final Observation obs1 = new Observation("1", System.currentTimeMillis() - 4320000);
    final Observation obs2 = new Observation("2", System.currentTimeMillis() - 4300000);
    observations.add(obs1);
    observations.add(obs2);

    return observations;
  }

  private List<SensorObservations> getSensorObservations() {
    final List<SensorObservations> sensorObservations = new ArrayList<SensorObservations>();
    final SensorObservations sensorObs1 = new SensorObservations(SENSOR1, "45.3 56.76");
    sensorObs1.setObservations(getObservations());
    final SensorObservations sensorObs2 = new SensorObservations(SENSOR2);
    sensorObs2.setObservations(getObservations());

    sensorObservations.add(sensorObs1);
    sensorObservations.add(sensorObs2);

    return sensorObservations;
  }
}
