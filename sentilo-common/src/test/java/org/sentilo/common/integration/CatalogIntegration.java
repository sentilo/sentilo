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
package org.sentilo.common.integration;

import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.common.rest.RESTClient;
import org.sentilo.common.rest.RequestContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.StringUtils;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:spring/catalog-integration-context.xml")
public class CatalogIntegration {

  @Autowired
  private RESTClient httpRestClient;

  @Test
  public void validateApiPermissions() {
    final String uri = "api/permissions";
    final String response = httpRestClient.get(new RequestContext(uri));
    // A response example would be:
    // {"permissions":[{"source":"prov1","target":"prov1","type":"WRITE"},{"source":"prov2","target":"prov2","type":"WRITE"}]}
    assertTrue("There was an error processing request to get api permissions", StringUtils.hasText(response));
    assertTrue("Response does not match expected format",
        response.contains("permissions") && response.contains("source") && response.contains("target") && response.contains("type"));
  }

  @Test
  public void validateApiAuth() {
    final String uri = "api/credentials";
    final String response = httpRestClient.get(new RequestContext(uri));
    // A response example would be
    // {"authorizations":[{"entity":"app1","token":"token3"},{"entity":"app2","token":"token4"},{"entity":"app3","token":"token5"}]}
    assertTrue("There was an error processing request to get api credentials", StringUtils.hasText(response));
    assertTrue("Response does not match expected format",
        response.contains("authorizations") && response.contains("entity") && response.contains("token"));
  }

}
