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
package org.sentilo.platform.server.converter;

import java.io.ByteArrayOutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.http.entity.ContentType;
import org.sentilo.common.converter.BaseJsonMessageConverter;
import org.sentilo.common.exception.MessageNotWritableException;
import org.sentilo.common.utils.DateUtils;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.server.request.SentiloRequest;
import org.sentilo.platform.server.response.SentiloResponse;
import org.springframework.util.StringUtils;

public class PlatformJsonMessageConverter extends BaseJsonMessageConverter {

  public static final Charset DEFAULT_CHARSET = Charset.forName("UTF-8");
  public static final ContentType DEFAULT_CONTENT_TYPE = ContentType.APPLICATION_JSON;
  public static final String UNMARSHAL_JSON_ERROR_TEMPLATE =
      "%s  Bad request data: could not read JSON payload. Please review the following error and try again";
  public static final String UNMARSHAL_JSON_FIELD_ERROR_TEMPLATE = "Wrong %s value: %s";
  public static final String MARSHAL_JSON_ERROR_TEMPLATE = SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE;

  protected void writeInternal(final Object obj, final SentiloResponse response) throws JsonConverterException {
    try {
      final ByteArrayOutputStream out = super.writeInternal(obj);
      response.setBody(out, DEFAULT_CONTENT_TYPE);
    } catch (final MessageNotWritableException ex) {
      throw buildMarshalJsonException(obj, ex);
    }
  }

  protected Object readInternal(final Class<?> clazz, final SentiloRequest request) throws JsonConverterException {
    try {
      return super.readInternal(clazz, request.getBody());
    } catch (final Exception ex) {
      throw buildUnmarshallJsonException(clazz, ex);
    }
  }

  /**
   * Normalize to UTC input timestamps.
   *
   * @param timestamp
   * @return
   * @throws JsonConverterException
   */
  protected Long parseTimestamp(final String timestamp) throws JsonConverterException {
    try {
      return DateUtils.parseTimestamp(timestamp);
    } catch (final IllegalArgumentException e) {
      throw buildJsonFieldError("timestamp", timestamp, e);
    }
  }

  protected Date parseDate(final String date) throws JsonConverterException {
    try {
      return DateUtils.stringToDate(date);
    } catch (final IllegalArgumentException e) {
      throw buildJsonFieldError("date", date, e);
    }
  }

  protected String parseLocation(final String location) throws JsonConverterException {
    if (SentiloUtils.isValidLocationFormat(location)) {
      return location;
    } else {
      throw buildJsonFieldError("location", location, new IllegalArgumentException(buildFieldErrorMessage("location", location)));
    }
  }

  protected String buildFieldErrorMessage(final String fieldName, final String fieldValue) {
    final String errorTemplate = "Wrong value for field %s (%s).";
    return String.format(errorTemplate, fieldName, fieldValue);
  }

  protected Integer parseInteger(final String integer) throws JsonConverterException {
    try {
      return StringUtils.hasText(integer) ? Integer.valueOf(integer) : null;
    } catch (final NumberFormatException e) {
      throw buildJsonFieldError("integer", integer, e);
    }
  }

  protected JsonConverterException buildJsonFieldError(final String type, final String value, final Throwable cause) {
    final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.JSON_UNMARSHAL_ERROR);
    getLogger().error("{} - Error unmarshalling JSON payload. Wrong {} value: {}.", internalErrorCode, type, value, cause);
    final String errorMessage = String.format(UNMARSHAL_JSON_ERROR_TEMPLATE, internalErrorCode);
    final List<String> errorDetails = new ArrayList<String>();
    errorDetails.add(String.format(UNMARSHAL_JSON_FIELD_ERROR_TEMPLATE, type, value));

    return new JsonConverterException(errorMessage, errorDetails);
  }

  protected JsonConverterException buildMarshalJsonException(final Object obj, final Throwable cause) {
    final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.JSON_MARSHAL_ERROR);
    getLogger().error("{} - Error marshalling object of type {} to JSON.", internalErrorCode, obj.getClass().getName(), cause);

    final String errorMessage = String.format(MARSHAL_JSON_ERROR_TEMPLATE, internalErrorCode);
    return new JsonConverterException(errorMessage);
  }

  protected JsonConverterException buildUnmarshallJsonException(final Class<?> clazz, final Throwable cause) {
    final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.JSON_UNMARSHAL_ERROR);
    getLogger().error("{} - Error unmarshalling JSON payload to class {}.", internalErrorCode, clazz.getName(), cause);

    final String errorMessage = String.format(UNMARSHAL_JSON_ERROR_TEMPLATE, internalErrorCode);
    return new JsonConverterException(errorMessage, cause);
  }

  public String timestampToString(final Long timestamp) {
    return DateUtils.timestampToString(timestamp);
  }
}
