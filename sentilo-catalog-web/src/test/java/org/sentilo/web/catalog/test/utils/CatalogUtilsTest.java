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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.Location;
import org.sentilo.web.catalog.domain.Sensor;
import org.sentilo.web.catalog.dto.OptionDTO;
import org.sentilo.web.catalog.utils.CatalogUtils;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.context.support.StaticMessageSource;
import org.springframework.util.StringUtils;

public class CatalogUtilsTest {

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  enum OrderEnum {
    ZERO, PETER, Anthony, THREE, AMBER, CHRISTIE, JULIAN, jackes, Jarry, peter
  };

  enum MockValuesEnum {
    CaÇA, BÖHM, _1234, CASA, BORROW, BeAn
  }

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void decodeAjaxParamNull() {
    final String source = null;
    final String result = CatalogUtils.decodeAjaxParam(source);
    Assert.assertNull(result);
  }

  @Test
  public void decodeAjaxParamEmpty() {
    final String source = "";
    final String expected = "";
    final String result = CatalogUtils.decodeAjaxParam(source);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void decodeAjaxParamNonEmpty() {
    final String source = "simple=hello&composed=one+two+three";
    final String expected = "simple=hello&composed=one two three";
    final String result = CatalogUtils.decodeAjaxParam(source);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void convertStringPointLocationaaa() {
    final String coordinates = "2.1234541.123456";

    final Location location = CatalogUtils.convertStringLocation(coordinates);

    Assert.assertTrue(location.getNumberOfCoordinates() == 1);
    Assert.assertEquals(null, location.getCoordinates()[0]);
  }

  @Test
  public void convertStringPointLocation() {
    final String coordinates = "2.12345 41.123456";

    final Location location = CatalogUtils.convertStringLocation(coordinates);

    Assert.assertTrue(location.getNumberOfCoordinates() == 1);
    Assert.assertEquals(coordinates, location.toString());
  }

  @Test
  public void convertStringPolygonLocation() {
    final String coordinates = "2.12345 41.123456,2.12345 41.123457,2.12346 41.123461";

    final Location location = CatalogUtils.convertStringLocation(coordinates);

    Assert.assertTrue(location.getNumberOfCoordinates() == 3);
    Assert.assertEquals(coordinates, location.toString());
  }

  @Test
  public void locationToString() {
    final String coordinates = "2.12345 41.123456,2.12345 41.123457,2.12346 41.123461";
    final Location location = CatalogUtils.convertStringLocation(coordinates);

    Assert.assertEquals(coordinates, CatalogUtils.locationToString(location));
  }

  @Test
  public void nullLocationToString() {
    Assert.assertNull(CatalogUtils.locationToString(null));
  }

  @Test
  public void getMaxSystemStringDate() {
    final Date maxDate = new Date(Long.MAX_VALUE);
    final Long expected = maxDate.getTime();
    final Long result = CatalogUtils.getMaxSystemTimeMillis();
    Assert.assertEquals(expected, result);
  }

  @Test
  public void isJSONValidFalse() {
    final String json = "I'm not a valid JSON";
    final boolean expected = false;
    final boolean result = CatalogUtils.isJSONValid(json);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void isJSONValidTrue() {
    final String json = "{\"id\":12345,\"name\":\"The name\"}";
    final boolean expected = true;
    final boolean result = CatalogUtils.isJSONValid(json);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void emptyMapToStringTest() {
    final String expected = "";
    final Map<String, String> map = new HashMap<String, String>();
    final String result = CatalogUtils.mapToString(map);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void stringMapToStringTest() {

    // Expected result is some String like this >>> "a=b; b=c; c=e; 1=2; 3=4; 5=6";

    final Map<String, String> map = new HashMap<String, String>();

    map.put("a", "b");
    map.put("b", "c");
    map.put("c", "e");
    map.put("1", "2");
    map.put("3", "4");
    map.put("5", "6");

    final String result = CatalogUtils.mapToString(map);

    final String[] splittedResult = result.split("; ");

    Assert.assertEquals(map.size(), splittedResult.length);
    Assert.assertTrue(result.contains("a=b"));
    Assert.assertTrue(result.contains("b=c"));
    Assert.assertTrue(result.contains("c=e"));
    Assert.assertTrue(result.contains("1=2"));
    Assert.assertTrue(result.contains("3=4"));
    Assert.assertTrue(result.contains("5=6"));
  }

  @Test
  public void objectMapToStringTest() {

    // Expected result is some String like this >>> "integer=12345, double=12.345, class=class
    // java.lang.String";

    final Map<String, Object> map = new HashMap<String, Object>();

    map.put("integer", new Integer(12345));
    map.put("double", new Double(12.345));
    map.put("class", String.class);

    final String result = CatalogUtils.mapToString(map);

    Assert.assertEquals(map.size(), StringUtils.countOccurrencesOf(result, "; ") + 1);
    Assert.assertTrue(result.contains("integer=12345"));
    Assert.assertTrue(result.contains("double=12.345"));
    Assert.assertTrue(result.contains("class=class java.lang.String"));
  }

  @Test
  public void emptyCollectionToString() {
    final String expected = "";
    final Collection<String> collection = new HashSet<String>();
    final String result = CatalogUtils.collectionToString(collection);
    Assert.assertEquals(expected, result);
  }

  @Test
  public void stringCollectionToString() {

    // Expected result is some String like this >>> "a, b, c, 1, 2, 3";

    final Collection<String> collection = new HashSet<String>();

    collection.add("a");
    collection.add("b");
    collection.add("c");
    collection.add("1");
    collection.add("2");
    collection.add("3");

    final String result = CatalogUtils.collectionToString(collection);

    Assert.assertEquals(collection.size(), StringUtils.countOccurrencesOf(result, ", ") + 1);
    Assert.assertTrue(result.contains("a"));
    Assert.assertTrue(result.contains("b"));
    Assert.assertTrue(result.contains("c"));
    Assert.assertTrue(result.contains("1"));
    Assert.assertTrue(result.contains("2"));
    Assert.assertTrue(result.contains("3"));
  }

  @Test
  public void objectCollectionToString() {

    // Expected result is some String like this >>> "12345, 12.345, class java.lang.String";

    final Collection<Object> collection = new HashSet<Object>();

    collection.add(new Integer(12345));
    collection.add(new Double(12.345));
    collection.add(String.class);

    final String result = CatalogUtils.collectionToString(collection);

    Assert.assertEquals(collection.size(), StringUtils.countOccurrencesOf(result, ", ") + 1);
    Assert.assertTrue(result.contains("12345"));
    Assert.assertTrue(result.contains("12.345"));
    Assert.assertTrue(result.contains("class java.lang.String"));
  }

  @Test
  public void arrayToString() {
    final String[] values = {"a", "b", "c"};
    final String[] values2 = {};

    final String result = CatalogUtils.arrayToString(values);
    final String result2 = CatalogUtils.arrayToString(values2);

    Assert.assertEquals("a, b, c", result);
    Assert.assertEquals("", result2);
  }

  @Test
  public void tagsToStringListNull() {
    final String tags = null;
    final List<String> result = CatalogUtils.tagsToStringList(tags);
    Assert.assertNull(result);
  }

  @Test
  public void tagsToStringListNotNull() {
    final String tags = "hola,adeu,demà,avui";
    final List<String> expected = Arrays.asList("hola", "adeu", "demà", "avui");
    final List<String> result = CatalogUtils.tagsToStringList(tags);
    Assert.assertTrue(result.equals(expected));
  }

  @Test
  public void sortLexicographically() {
    final Sensor s1 = new Sensor("mockProvider.mockComponent1.sensor21");
    final Sensor s2 = new Sensor("mockProvider.mockComponent2.sensor_pruebas");
    final Sensor s3 = new Sensor("mockProvider.mockComponent3.Sensor13");
    final Sensor s4 = new Sensor("mockProvider.mockComponent4.SenSOr22");

    final Sensor[] sensors = {s1, s2, s3, s4};
    final List<Sensor> sortedList = CatalogUtils.sortAlphabetically(Arrays.asList(sensors));

    Assert.assertTrue(sortedList.size() == sensors.length);
    Assert.assertEquals(s2, sortedList.get(0));
    Assert.assertEquals(s3, sortedList.get(1));
    Assert.assertEquals(s1, sortedList.get(2));
    Assert.assertEquals(s4, sortedList.get(3));

  }

  @Test
  public void resourcesToOptionList() {
    final Sensor s1 = new Sensor("mockProvider.mockComponent1.sensor21");
    final Sensor s2 = new Sensor("mockProvider.mockComponent2.sensor_pruebas");
    final Sensor s3 = new Sensor("mockProvider.mockComponent3.Sensor13");
    final Sensor s4 = new Sensor("mockProvider.mockComponent4.SenSOr22");

    final Sensor[] sensors = {s1, s2, s3, s4};

    final List<OptionDTO> options = CatalogUtils.toOptionList(Arrays.asList(sensors));

    Assert.assertTrue(options.size() == sensors.length);
    Assert.assertEquals(s2.getId(), options.get(0).getValue());
    Assert.assertEquals(s2.getSortableValue(), options.get(0).getLabel());
    Assert.assertEquals(s4.getId(), options.get(3).getValue());
    Assert.assertEquals(s4.getSortableValue(), options.get(3).getLabel());
  }

  @Test
  public void enumGetNames() {
    final String[] expected = {"CaÇA", "BÖHM", "_1234", "CASA", "BORROW", "BeAn"};
    final String[] names = CatalogUtils.enumGetNames(MockValuesEnum.class);

    Assert.assertTrue(Arrays.equals(expected, names));
  }

  @Test
  public void enumToOptionList() {
    final List<OptionDTO> options = CatalogUtils.toOptionList(MockValuesEnum.class, "mock.prefix", buildMockMessageSource());

    Assert.assertTrue(options.size() == MockValuesEnum.values().length);
    Assert.assertEquals(MockValuesEnum._1234.name(), options.get(0).getValue());
    Assert.assertEquals("Label " + MockValuesEnum._1234.name(), options.get(0).getLabel());
    Assert.assertEquals(MockValuesEnum.BeAn.name(), options.get(1).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.BeAn.name(), options.get(1).getLabel());
    Assert.assertEquals(MockValuesEnum.CASA.name(), options.get(5).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.CASA.name(), options.get(5).getLabel());
  }

  @Test
  public void stringArrayToOptionList() {
    final String[] values = CatalogUtils.enumGetNames(MockValuesEnum.class);
    final List<OptionDTO> options = CatalogUtils.toOptionList(values, "mock.prefix", buildMockMessageSource());

    Assert.assertTrue(options.size() == MockValuesEnum.values().length);
    Assert.assertEquals(MockValuesEnum._1234.name(), options.get(0).getValue());
    Assert.assertEquals("Label " + MockValuesEnum._1234.name(), options.get(0).getLabel());
    Assert.assertEquals(MockValuesEnum.BeAn.name(), options.get(1).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.BeAn.name(), options.get(1).getLabel());
    Assert.assertEquals(MockValuesEnum.CASA.name(), options.get(5).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.CASA.name(), options.get(5).getLabel());
  }

  @Test
  public void stringToOptionList() {
    final String values = "CaÇA, BÖHM, _1234, CASA, BORROW, BeAn";
    final List<OptionDTO> options = CatalogUtils.toOptionList(values, "mock.prefix", buildMockMessageSource());

    Assert.assertTrue(options.size() == MockValuesEnum.values().length);
    Assert.assertEquals(MockValuesEnum._1234.name(), options.get(0).getValue());
    Assert.assertEquals("Label " + MockValuesEnum._1234.name(), options.get(0).getLabel());
    Assert.assertEquals(MockValuesEnum.BeAn.name(), options.get(1).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.BeAn.name(), options.get(1).getLabel());
    Assert.assertEquals(MockValuesEnum.CASA.name(), options.get(5).getValue());
    Assert.assertEquals("Label " + MockValuesEnum.CASA.name(), options.get(5).getLabel());
  }

  @Test
  public void isStaticRequest() {
    final String requestUri1 = "/sentilo-catalog-web/static/tenant/mockTenant/img/logo_header.png";
    final String requestUri2 = "/sentilo-catalog-web/stats";

    when(request.getRequestURI()).thenReturn(requestUri1).thenReturn(requestUri2);
    when(request.getContextPath()).thenReturn("sentilo-catalog-web");

    final boolean result1 = CatalogUtils.isStaticRequest(request);
    final boolean result2 = CatalogUtils.isStaticRequest(request);

    Assert.assertTrue(result1);
    Assert.assertFalse(result2);
  }

  private MessageSource buildMockMessageSource() {
    final StaticMessageSource messageSource = new StaticMessageSource();
    messageSource.addMessage("mock.prefix." + MockValuesEnum.CaÇA.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum.CaÇA.name());
    messageSource.addMessage("mock.prefix." + MockValuesEnum.CASA.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum.CASA.name());
    messageSource.addMessage("mock.prefix." + MockValuesEnum.BeAn.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum.BeAn.name());
    messageSource.addMessage("mock.prefix." + MockValuesEnum._1234.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum._1234.name());
    messageSource.addMessage("mock.prefix." + MockValuesEnum.BORROW.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum.BORROW.name());
    messageSource.addMessage("mock.prefix." + MockValuesEnum.BÖHM.name(), LocaleContextHolder.getLocale(), "Label " + MockValuesEnum.BÖHM.name());

    return messageSource;
  }

}
