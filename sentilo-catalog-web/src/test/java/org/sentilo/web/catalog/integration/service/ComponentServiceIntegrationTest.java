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
package org.sentilo.web.catalog.integration.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.web.catalog.domain.Component;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.service.ComponentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ActiveProfiles("dev")
@ContextConfiguration(locations = "classpath:spring/test-mongodb-service-context.xml")
public class ComponentServiceIntegrationTest {

  @Autowired
  private ComponentService componentService;

  @Autowired
  private MongoOperations mongoOps;

  @Test
  public void searchWithFilters() {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("mobile", 0);

    final List<Component> resources = componentService.search(filter).getContent();
    assertTrue("Found " + resources.size() + " and must be greater than 0", resources.size() >= 1);
  }

  @Test
  public void searchBetweenDates() {
    final Date startDate = getDate(-60);
    final Date endDate = getDate(0);
    final Query query = new Query(Criteria.where("updatedAt").lte(endDate).gte(startDate));

    final List<Component> components = mongoOps.find(query, Component.class);
    assertTrue(components.size() >= 0);
  }

  @Test
  public void insertAndUpdate() {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", "TEST");

    assertTrue(componentService.search(filter).getContent().size() == 0);

    final List<Component> componentsToInsert = getComponentsToInsert();
    componentService.insertAll(componentsToInsert);
    assertTrue(componentService.search(filter).getContent().size() == componentsToInsert.size());

    final List<Component> componentsToUpdate = getComponentsToUpdate();
    componentService.updateAll(componentsToUpdate);
    final List<Component> components = componentService.search(filter).getContent();
    for (final Component component : components) {
      assertNotNull(component.getDescription());
      assertTrue(component.getPublicAccess());
    }

    componentService.delete(components);
    assertTrue(componentService.search(filter).getContent().size() == 0);
  }

  @Test
  public void insertAndUpdateWithErrors() {
    final SearchFilter filter = new SearchFilter();
    filter.addAndParam("providerId", "TEST");

    assertTrue(componentService.search(filter).getContent().size() == 0);

    final List<Component> componentsToInsert = getComponentsToInsert();
    componentService.insertAll(componentsToInsert);
    assertTrue(componentService.search(filter).getContent().size() == componentsToInsert.size());

    final List<Component> componentsToUpdate = getComponentsToUpdateWithErrors();
    componentService.updateAll(componentsToUpdate);
    final List<Component> components = componentService.search(filter).getContent();

    assertTrue(componentService.search(filter).getContent().size() == 2);

    for (final Component component : components) {
      assertNotNull(component.getDescription());
      assertTrue(component.getPublicAccess());
    }

    componentService.delete(components);
    assertTrue(componentService.search(filter).getContent().size() == 0);
  }

  private Date getDate(final int daysToSubstract) {
    final Calendar calendar = Calendar.getInstance(new Locale("ca", "ES"));
    calendar.setTime(new Date());
    calendar.add(Calendar.DATE, daysToSubstract);

    return calendar.getTime();
  }

  private List<Component> getComponentsToInsert() {
    final String providerId = "TEST";
    final Component comp1 = new Component();
    comp1.setProviderId(providerId);
    comp1.setName("TEST1");
    comp1.setPublicAccess(false);

    final Component comp2 = new Component();
    comp2.setProviderId(providerId);
    comp2.setName("TEST2");
    comp2.setPublicAccess(true);

    final List<Component> components = new ArrayList<Component>();
    components.add(comp1);
    components.add(comp2);

    return components;
  }

  private List<Component> getComponentsToUpdate() {
    final String providerId = "TEST";
    final Component comp1 = new Component();
    comp1.setProviderId(providerId);
    comp1.setName("TEST1");
    comp1.setPublicAccess(true);
    comp1.setDescription("Desc de TEST1");

    final Component comp2 = new Component();
    comp2.setProviderId(providerId);
    comp2.setName("TEST2");
    comp2.setPublicAccess(true);
    comp2.setDescription("Desc de TEST2");

    final List<Component> components = new ArrayList<Component>();
    components.add(comp1);
    components.add(comp2);

    return components;
  }

  private List<Component> getComponentsToUpdateWithErrors() {
    final String providerId = "TEST";
    final Component comp1 = new Component();
    comp1.setProviderId(providerId);
    comp1.setName("TEST1");
    comp1.setPublicAccess(true);
    comp1.setDescription("Desc de TEST1");

    final Component comp2 = new Component();
    comp2.setProviderId(providerId);
    comp2.setName("TEST2");
    comp2.setPublicAccess(true);
    comp2.setDescription("Desc de TEST2");

    final Component comp3 = new Component();
    comp3.setProviderId(providerId);
    comp3.setId(providerId + ".TEST3");
    comp3.setPublicAccess(true);
    comp3.setDescription("Desc de TEST3");

    final List<Component> components = new ArrayList<Component>();
    components.add(comp1);
    components.add(comp2);
    components.add(comp3);

    return components;
  }

}
