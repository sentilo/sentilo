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
package org.sentilo.platform.server.parser;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;

import org.apache.http.entity.ContentType;
import org.sentilo.common.exception.MessageNotReadableException;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.common.parser.BaseJsonMessageConverter;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;

public class PlatformJsonMessageConverter extends BaseJsonMessageConverter {

  public static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");
  public static final ContentType DEFAULT_CONTENT_TYPE = ContentType.APPLICATION_JSON;

  protected void writeInternal(final Object o, final SentiloResponse response) throws IOException, JsonConverterException {
    try {
      final ByteArrayOutputStream out = super.writeInternal(o);
      response.setBody(out, DEFAULT_CONTENT_TYPE);
    } catch (final MessageNotWritableException ex) {
      throw new JsonConverterException("Could not write JSON: " + ex.getMessage(), ex, false);
    }
  }

  protected Object readInternal(final Class<?> clazz, final SentiloRequest request) throws JsonConverterException {
    try {
      return super.readInternal(clazz, request.getBody());
    } catch (final IOException ex) {
      throw new JsonConverterException("Could not write JSON: " + ex.getMessage(), ex, true);
    } catch (final MessageNotReadableException mne) {
      throw new JsonConverterException("Could not write JSON: " + mne.getMessage(), mne, true);
    }
  }

  public Long parseTimestamp(final String timestamp) throws JsonConverterException {
    try {
      return DateUtils.parseTimestamp(timestamp);
    } catch (final IllegalArgumentException e) {
      throw new JsonConverterException("Error while parsing timestamp " + timestamp, e, true);
    }
  }

  public Date parseDate(final String date) throws JsonConverterException {
    try {
      return DateUtils.stringToDate(date);
    } catch (final IllegalArgumentException e) {
      throw new JsonConverterException("Error while parsing date " + date, e, true);
    }
  }

  public Integer parseInteger(final String integer) throws JsonConverterException {
    try {
      return (StringUtils.hasText(integer) ? Integer.valueOf(integer) : null);
    } catch (final NumberFormatException e) {
      throw new JsonConverterException("Error while parsing integer " + integer, e, true);
    }
  }

  public String timestampToString(final Long timestamp) {
    return DateUtils.timestampToString(timestamp);
  }
}
