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
package org.sentilo.web.catalog.test.service;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.regex.Pattern;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.search.SearchFilter;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.util.ReflectionUtils;

public class AbstractBaseServiceImplTest<T> {

  @InjectMocks
  private MockBaseServiceImpl service;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testBuildQuerySearchFilter() throws Exception {
    final SearchFilter filter = new SearchFilter();
    filter.addParam("mobile", 1);
    filter.addParam("tset", "tset");
    filter.addParam("statusTrue", Boolean.TRUE);

    final Method buildOrParamsCriteria = ReflectionUtils.findMethod(MockBaseServiceImpl.class, "buildOrParamsCriteria", new Class[] {Map.class});
    ReflectionUtils.makeAccessible(buildOrParamsCriteria);

    final Criteria[] crit = (Criteria[]) buildOrParamsCriteria.invoke(service, filter.getParams());

    Assert.assertNotNull(crit);
    Assert.assertTrue(crit.length == filter.getParams().size());

    for (final Criteria criteria : crit) {
      if (criteria.getKey().equals("mobile")) {
        Assert.assertEquals(1, criteria.getCriteriaObject().get("mobile"));
      } else if (criteria.getKey().equals("statusTrue")) {
        Assert.assertTrue((Boolean) criteria.getCriteriaObject().get("statusTrue"));
      } else if (criteria.getKey().equals("tset")) {
        Assert.assertTrue(criteria.getCriteriaObject().get("tset") instanceof Pattern);
      }
    }
  }

}
