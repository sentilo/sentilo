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
package org.sentilo.web.catalog.test.security.crypto;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.sentilo.web.catalog.security.crypto.SentiloDelegatingPasswordEncoder;
import org.springframework.test.util.ReflectionTestUtils;

public class SentiloDelegatingPasswordEncoderTest {

  private final String RAW_PASSWORD = "12345-Password";
  private final String ENCODED_PASSWORD = "$2a$07$jWsjUnukomcBjeQHGoEc8eVH0P1PTzhAS7XxNx56ya1txWdv8tL8K";

  private SentiloDelegatingPasswordEncoder passwordEncoder;

  @Before
  public void setUp() {
    passwordEncoder = new SentiloDelegatingPasswordEncoder();
  }

  @Test
  public void defaultBehaviour() {
    passwordEncoder.init();

    Assert.assertTrue(passwordEncoder.encodePasswords());
    Assert.assertTrue(passwordEncoder.matches(RAW_PASSWORD, ENCODED_PASSWORD));
  }

  @Test
  public void bcryptEncoder() {
    ReflectionTestUtils.setField(passwordEncoder, "idAlgorithmForEncode", "bcrypt");
    passwordEncoder.init();

    Assert.assertTrue(passwordEncoder.encodePasswords());
    Assert.assertTrue(passwordEncoder.matches(RAW_PASSWORD, ENCODED_PASSWORD));
  }

  @Test
  public void noopEncoder() {
    ReflectionTestUtils.setField(passwordEncoder, "idAlgorithmForEncode", "noop");
    passwordEncoder.init();

    Assert.assertTrue(!passwordEncoder.encodePasswords());
    Assert.assertTrue(passwordEncoder.matches(RAW_PASSWORD, RAW_PASSWORD));
  }

  @Test
  public void unknownEncoder() {
    ReflectionTestUtils.setField(passwordEncoder, "idAlgorithmForEncode", "xyz");
    passwordEncoder.init();

    Assert.assertTrue(passwordEncoder.encodePasswords());
    Assert.assertTrue(passwordEncoder.matches(RAW_PASSWORD, ENCODED_PASSWORD));
  }
}
