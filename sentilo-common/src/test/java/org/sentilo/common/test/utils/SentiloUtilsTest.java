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
package org.sentilo.common.test.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.sentilo.common.utils.SentiloUtils;

public class SentiloUtilsTest {

  @Test
  public void arrayIsEmpty() {
    final String[] emptyArray = {};
    final String[] noEmptyArray = {"a", "b", "c"};
    assertTrue(SentiloUtils.arrayIsEmpty(emptyArray));
    assertFalse(SentiloUtils.arrayIsEmpty(noEmptyArray));
  }

  @Test
  public void stringIsNotEmptyOrNull() {
    assertFalse(SentiloUtils.stringIsNotEmptyOrNull(null));
    assertFalse(SentiloUtils.stringIsNotEmptyOrNull("null"));
    assertFalse(SentiloUtils.stringIsNotEmptyOrNull("NULL"));
    assertFalse(SentiloUtils.stringIsNotEmptyOrNull(" "));
    assertFalse(SentiloUtils.stringIsNotEmptyOrNull(""));
    assertTrue(SentiloUtils.stringIsNotEmptyOrNull("abc"));

  }

  @Test
  public void buildNewInternalErrorCode() {
    final String prefix = "CRC";
    assertTrue(SentiloUtils.buildNewInternalErrorCode(prefix).startsWith(prefix));
  }

  @Test
  public void addValuesToBeginningList() {
    final String[] baseList = {"a", "b", "c", "d"};
    final String[] values = {"1", "2", "3"};
    final String[] resultExpected = {"1", "2", "3", "a", "b", "c", "d"};

    assertEquals(Arrays.asList(resultExpected), SentiloUtils.addValuesToBeginningList(Arrays.asList(baseList), values));
  }

  @Test
  public void addEmptyValuesToBeginningList() {
    final String[] baseList = {"a", "b", "c", "d"};
    final String[] values = {};

    assertEquals(Arrays.asList(baseList), SentiloUtils.addValuesToBeginningList(Arrays.asList(baseList), values));
  }

  @Test
  public void isArrayOrCollection() {
    final String var1 = "a";
    final Integer var2 = new Integer(1);
    final String[] var3 = {"a", "b"};
    final List<String> var4 = Arrays.asList(var3);
    final Object var5 = null;

    assertFalse(SentiloUtils.isArrayOrCollection(var1));
    assertFalse(SentiloUtils.isArrayOrCollection(var2));
    assertTrue(SentiloUtils.isArrayOrCollection(var3));
    assertTrue(SentiloUtils.isArrayOrCollection(var4));
    assertFalse(SentiloUtils.isArrayOrCollection(var5));

  }

  @Test
  public void areEquals() {
    assertTrue(SentiloUtils.areEquals(null, null));
    assertTrue(SentiloUtils.areEquals("", ""));
    assertFalse(SentiloUtils.areEquals(null, ""));
    assertFalse(SentiloUtils.areEquals("", null));
    assertFalse(SentiloUtils.areEquals("a", "b"));
    assertTrue(SentiloUtils.areEquals("a", "a"));
    assertFalse(SentiloUtils.areEquals("a", "a "));

  }

  @Test
  public void isValidLocationFormat() {
    final String location = "1.123456 4.435678";
    final String location2 = "1.123456 4.435678, 1.123424 4.435689";
    final String location3 = "1.123456 4.435678, 2.34567, 1.123424 4.435689";
    final String location4 = "1.123456 4.435678, 1.abc3424 4.435689";
    final String location5 = ",41.613624,2.291772,";

    assertTrue(SentiloUtils.isValidLocationFormat(location));
    assertTrue(SentiloUtils.isValidLocationFormat(location2));
    assertFalse(SentiloUtils.isValidLocationFormat(location3));
    assertFalse(SentiloUtils.isValidLocationFormat(location4));
    assertFalse(SentiloUtils.isValidLocationFormat(location5));

  }
}
