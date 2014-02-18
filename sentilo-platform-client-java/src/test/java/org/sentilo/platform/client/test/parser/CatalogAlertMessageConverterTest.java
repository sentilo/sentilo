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
package org.sentilo.platform.client.test.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.domain.CatalogAlert;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.platform.client.core.domain.CatalogAlertInputMessage;
import org.sentilo.platform.client.core.domain.CatalogAlertOutputMessage;
import org.sentilo.platform.client.core.parser.CatalogAlertMessageConverter;
import org.springframework.util.CollectionUtils;

public class CatalogAlertMessageConverterTest {

  static final String ENTITY_ID = "prov1";

  CatalogAlertMessageConverter converter = new CatalogAlertMessageConverter();

  @Test
  public void buildBody() throws MessageNotWritableException {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    message.setAlerts(getAlerts());
    final String json = converter.marshall(message);
    final String body =
        "{\"alerts\":[{\"id\":\"ALERT012\",\"name\":\"ALERT012\",\"description\":\"alert 12\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"},"
            + "{\"id\":\"ALERT013\",\"name\":\"ALERT013\",\"description\":\"alert 13\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"}]}";

    assertNotNull(json);
    assertEquals(body, json);
  }

  @Test
  public void marshallEntityDeleteRequest() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    final String json = converter.marshall(message);
    assertEquals("{}", json);
  }

  @Test
  public void marshallAlertsDeleteRequest() {
    final CatalogAlertInputMessage message = new CatalogAlertInputMessage(ENTITY_ID);
    final String[] alertsIds = {"1", "2"};
    message.setAlertsIds(alertsIds);
    final String json = converter.marshall(message);
    final String expectedJson = "{\"alertsIds\":[\"1\",\"2\"]}";
    assertEquals(expectedJson, json);

  }

  @Test
  public void unmarshall() {
    final String response =
        "{\"alerts\":[{\"id\":\"ALERT012\",\"name\":\"ALERT012\",\"description\":\"alert 12\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"},"
            + "{\"id\":\"ALERT013\",\"name\":\"ALERT013\",\"description\":\"alert 13\",\"entity\":\"prov1\",\"type\":\"EXTERNAL\"}]}";
    final CatalogAlertOutputMessage outputMessage = converter.unmarshall(response);
    assertNotNull(outputMessage);
    assertTrue(!CollectionUtils.isEmpty(outputMessage.getAlerts()));
    assertEquals(2, outputMessage.getAlerts().size());
    assertEquals("ALERT012", outputMessage.getAlerts().get(0).getId());

  }

  private List<CatalogAlert> getAlerts() {
    final List<CatalogAlert> alerts = new ArrayList<CatalogAlert>();
    final CatalogAlert alert1 = buildAlert("ALERT012", "ALERT012", "alert 12", ENTITY_ID);
    final CatalogAlert alert2 = buildAlert("ALERT013", "ALERT013", "alert 13", ENTITY_ID);
    alerts.add(alert1);
    alerts.add(alert2);

    return alerts;
  }

  private CatalogAlert buildAlert(final String id, final String name, final String description, final String entity) {

    final CatalogAlert alert = new CatalogAlert();
    alert.setId(id);
    alert.setName(name);
    alert.setDescription(description);
    alert.setEntity(entity);
    alert.setType("EXTERNAL");

    return alert;
  }

}
