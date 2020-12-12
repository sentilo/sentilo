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
package org.sentilo.web.catalog.security.crypto;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.NoOpPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

/**
 * Based on {@link DelegatingPasswordEncoder}, this implementation doesn't append algorithm id to
 * the final encoded password and only allows switch between noop (no-password-encoded) and bcrypt
 * PasswordEncoders implementations.
 *
 */
@SuppressWarnings("deprecation")
@Component
public class SentiloDelegatingPasswordEncoder implements PasswordEncoder {

  private final static String DEFAULT_ENCODER_ID = "bcrypt";
  private static final int BCRYPT_DEFAULT_SALT = 7;

  @Value("${sentilo.crypto.password.algorithm.id:bcrypt}")
  private String idAlgorithmForEncode = DEFAULT_ENCODER_ID;

  private PasswordEncoder delegate;

  private static Map<String, PasswordEncoder> idToPasswordEncoder;

  static {
    idToPasswordEncoder = new HashMap<String, PasswordEncoder>();
    idToPasswordEncoder.put("noop", NoOpPasswordEncoder.getInstance());
    idToPasswordEncoder.put("bcrypt", new BCryptPasswordEncoder(BCRYPT_DEFAULT_SALT));
  }

  public SentiloDelegatingPasswordEncoder() {
    super();
  }

  @PostConstruct
  public void init() {
    switch (idAlgorithmForEncode) {
      case "noop":
      case "bcrypt":
        delegate = idToPasswordEncoder.get(idAlgorithmForEncode);
        break;
      default:
        delegate = idToPasswordEncoder.get("bcrypt");
        break;
    }
  }

  public boolean encodePasswords() {
    return !"noop".equals(idAlgorithmForEncode);
  }

  @Override
  public String encode(final CharSequence rawPassword) {
    return delegate.encode(rawPassword);
  }

  @Override
  public boolean matches(final CharSequence rawPassword, final String encodedPassword) {
    return delegate.matches(rawPassword, encodedPassword);
  }
}
