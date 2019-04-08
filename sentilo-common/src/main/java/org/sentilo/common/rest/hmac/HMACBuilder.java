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
package org.sentilo.common.rest.hmac;

import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Base64;
import org.apache.http.entity.ContentType;

/**
 * Utility class to build an HMAC value from request content to use as header in notification
 * callbacks.
 *
 */
public abstract class HMACBuilder {

  private static final String DIGEST_ALGORITHM = "MD5";
  private static final String MAC_ALGORITHM = "HmacSHA512";
  private static final String HTTP_VERB = "POST";
  private static final String TOKEN = "\n";

  private HMACBuilder() {
    throw new AssertionError();
  }

  public static String buildHeader(final String body, final String endpoint, final String secret, final String currentDate)
      throws GeneralSecurityException {
    final String contentMd5 = calculateMD5(body);
    final String toSign = getContentToSign(HTTP_VERB, contentMd5, ContentType.APPLICATION_JSON.getMimeType(), currentDate, endpoint);
    return calculateHMAC(secret, toSign);
  }

  public static boolean checkHeader(final String headerValue, final String body, final String endpoint, final String secret, final String currentDate)
      throws GeneralSecurityException {
    final String headerValueExpected = buildHeader(body, endpoint, secret, currentDate);
    return headerValue.equals(headerValueExpected);
  }

  private static String getContentToSign(final String... values) {
    final StringBuilder content = new StringBuilder();
    boolean first = true;

    for (final String value : values) {
      if (!first) {
        content.append(TOKEN);
      }

      content.append(value);

      first = false;
    }

    return content.toString();
  }

  private static String calculateHMAC(final String secret, final String data) throws GeneralSecurityException {
    final SecretKeySpec signingKey = new SecretKeySpec(secret.getBytes(), MAC_ALGORITHM);

    final Mac mac = Mac.getInstance(MAC_ALGORITHM);
    mac.init(signingKey);

    final byte[] rawHmac = mac.doFinal(data.getBytes());
    return new String(Base64.encodeBase64(rawHmac));
  }

  private static String calculateMD5(final String contentToEncode) throws NoSuchAlgorithmException {
    final MessageDigest digest = MessageDigest.getInstance(DIGEST_ALGORITHM);
    digest.reset();
    digest.update(contentToEncode.getBytes());
    return new String(Base64.encodeBase64(digest.digest()));
  }

}
