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
package org.sentilo.platform.server.request;

import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.List;

import org.apache.http.Header;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.util.EntityUtils;
import org.sentilo.common.enums.HttpHeader;
import org.sentilo.common.exception.PlatformAccessException;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.common.ratelimiter.QuotaContext;
import org.sentilo.platform.common.ratelimiter.QuotaContextHolder;
import org.sentilo.platform.common.security.RequesterContextHolder;
import org.sentilo.platform.server.converter.ErrorConverter;
import org.sentilo.platform.server.converter.PlatformJsonMessageConverter;
import org.sentilo.platform.server.dto.ErrorMessage;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.request.interceptor.SentiloRequestHandlerInterceptor;
import org.sentilo.platform.server.response.SentiloResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SentiloRequestHandler implements HttpRequestHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(SentiloRequestHandler.class);

  private final ErrorConverter errorParser;

  private HandlerLocator handlerLocator;

  private List<SentiloRequestHandlerInterceptor> requestInterceptors = Collections.emptyList();

  public SentiloRequestHandler() {
    super();
    errorParser = new ErrorConverter();
  }

  @Override
  public void handle(final HttpRequest httpRequest, final HttpResponse httpResponse, final HttpContext httpContext) {

    try {
      final SentiloRequest request = SentiloRequest.build(httpRequest, httpContext);
      final SentiloResponse response = SentiloResponse.build(httpResponse);
      debug(request);

      // Apply pre-handle treatments on entry request.
      applyPreHandle(request);

      final AbstractHandler handler = lookupHandlerForRequest(request);
      handler.manageRequest(request, response);

      // Prepare response and add additional headers, e.g., RL headers
      applyPostHandle(httpResponse, request.getContentType().toString());
    } catch (final PlatformException e) {
      final int errorCode = e.getHttpStatus() != 0 ? e.getHttpStatus() : HttpStatus.SC_INTERNAL_SERVER_ERROR;

      buildErrorResponse(httpResponse, errorCode, e.getMessage(), e.getErrorDetails());
    } catch (final PlatformAccessException e) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.SENTILO_ACCESS_ERROR);
      LOGGER.error("{} - Internal access error.", internalErrorCode, e);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      final int errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR;

      buildErrorResponse(httpResponse, errorCode, errorMessage);
    } catch (final Exception e) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.SENTILO_UNKNOWN_ERROR);
      LOGGER.error("{} - Internal server error.", internalErrorCode, e);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      final int errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR;

      buildErrorResponse(httpResponse, errorCode, errorMessage);
    } finally {
      debug(httpResponse);
      RequesterContextHolder.clearContext();
      QuotaContextHolder.clearContext();
    }

  }

  /**
   * Apply custom treatment of registered interceptors.
   */
  private void applyPreHandle(final SentiloRequest request) {
    for (final SentiloRequestHandlerInterceptor reqInterceptor : requestInterceptors) {
      reqInterceptor.invoke(request);
    }
  }

  private void buildErrorResponse(final HttpResponse response, final int errorCode, final String errorMessage) {
    buildErrorResponse(response, errorCode, errorMessage, Collections.<String>emptyList());
  }

  private void buildErrorResponse(final HttpResponse response, final int errorCode, final String errorMessage, final List<String> errorDetails) {
    final ErrorMessage message = new ErrorMessage(errorCode, errorMessage, errorDetails);

    try {
      final ByteArrayOutputStream baos = errorParser.writeInternal(message);
      response.setStatusCode(errorCode);
      response.setEntity(new ByteArrayEntity(baos.toByteArray(), PlatformJsonMessageConverter.DEFAULT_CONTENT_TYPE));
      addHeader(response, HttpHeader.CONTENT_TYPE, PlatformJsonMessageConverter.DEFAULT_CONTENT_TYPE.toString());
      addExtraResponseHeaders(response);
    } catch (final JsonConverterException jce) {
      response.setStatusCode(errorCode);
      response.setEntity(new ByteArrayEntity(jce.getMessage().getBytes()));
    }
  }

  private void applyPostHandle(final HttpResponse httpResponse, final String contentType) {
    httpResponse.setStatusCode(HttpStatus.SC_OK);
    addHeader(httpResponse, HttpHeader.CONTENT_TYPE, contentType);
    addExtraResponseHeaders(httpResponse);
  }

  private void addExtraResponseHeaders(final HttpResponse httpResponse) {
    // Add Rate Limiter headers. QuotaContextHolder may contain either global and/or account
    // QuotaContext
    if (QuotaContextHolder.hasGlobalContext()) {
      final QuotaContext qc = QuotaContextHolder.getContext(QuotaContext.Type.GLOBAL);
      addHeader(httpResponse, HttpHeader.X_RL_GLOBAL_INPUT_LIMIT, qc.getLimit());
      addHeader(httpResponse, HttpHeader.X_RL_GLOBAL_INPUT_REMAINING, qc.getRemaining());
      addHeader(httpResponse, HttpHeader.X_RL_GLOBAL_INPUT_RESET, qc.getMinutesToReset());
    }

    if (QuotaContextHolder.hasEntityContext()) {
      final QuotaContext qc = QuotaContextHolder.getContext(QuotaContext.Type.ENTITY);
      addHeader(httpResponse, HttpHeader.X_RL_INPUT_LIMIT, qc.getLimit());
      addHeader(httpResponse, HttpHeader.X_RL_INPUT_REMAINING, qc.getRemaining());
      addHeader(httpResponse, HttpHeader.X_RL_INPUT_RESET, qc.getMinutesToReset());
    }
  }

  private void addHeader(final HttpResponse httpResponse, final HttpHeader header, final Object value) {
    httpResponse.setHeader(header.toString(), value.toString());
  }

  private AbstractHandler lookupHandlerForRequest(final SentiloRequest request) throws PlatformException {
    LOGGER.debug("Looking handler for request {}", request.getUri());

    final AbstractHandler handler = handlerLocator.lookup(request);
    if (handler == null) {
      throw new PlatformException(HttpStatus.SC_NOT_FOUND, "handler not found to process request " + request.getUri());
    }
    return handler;
  }

  private void debug(final SentiloRequest request) {
    LOGGER.info("[remote_addr={}] ->  New http {} request: {}", request.getRemoteClientAddress(), request.getMethod(), request.getUri());
    LOGGER.debug("Content-Type: {}", request.getContentType());

  }

  private void debug(final HttpResponse httpResponse) {

    final int statusCode = httpResponse.getStatusLine() != null ? httpResponse.getStatusLine().getStatusCode() : -1;
    LOGGER.info("Return http response with status code: {}", statusCode);

    final Header[] header = httpResponse.getHeaders(HttpHeader.CONTENT_TYPE.toString());
    if (header != null && header.length > 0) {
      final String contentType = header[0].getValue();
      LOGGER.debug("{} : {}", HttpHeader.CONTENT_TYPE.toString(), contentType);
    }

    if (httpResponse.getEntity() != null) {
      try {
        LOGGER.trace("Entity body: {} ", EntityUtils.toString(httpResponse.getEntity()));
      } catch (final Exception e) {
        LOGGER.error("Error parsing body", e);
      }
    }

  }

  public void setHandlerLocator(final HandlerLocator handlerLocator) {
    this.handlerLocator = handlerLocator;
  }

  public void setRequestInterceptors(final List<SentiloRequestHandlerInterceptor> requestInterceptors) {
    this.requestInterceptors = requestInterceptors;
  }

}
