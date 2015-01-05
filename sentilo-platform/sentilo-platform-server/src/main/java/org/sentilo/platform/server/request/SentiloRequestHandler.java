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
import org.sentilo.common.exception.PlatformAccessException;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.platform.common.exception.JsonConverterException;
import org.sentilo.platform.common.exception.PlatformException;
import org.sentilo.platform.server.auth.AuthenticationService;
import org.sentilo.platform.server.dto.ErrorMessage;
import org.sentilo.platform.server.handler.AbstractHandler;
import org.sentilo.platform.server.handler.HandlerLocator;
import org.sentilo.platform.server.http.HttpHeader;
import org.sentilo.platform.server.parser.ErrorParser;
import org.sentilo.platform.server.parser.PlatformJsonMessageConverter;
import org.sentilo.platform.server.response.SentiloResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SentiloRequestHandler implements HttpRequestHandler {

  private final Logger logger = LoggerFactory.getLogger(SentiloRequestHandler.class);

  private final HandlerLocator handlerLocator;
  private final AuthenticationService authenticationService;
  private final ErrorParser errorParser;

  public SentiloRequestHandler(final HandlerLocator handlerLocator, final AuthenticationService authService) {
    this.handlerLocator = handlerLocator;
    authenticationService = authService;
    errorParser = new ErrorParser();
  }

  @Override
  public void handle(final HttpRequest httpRequest, final HttpResponse httpResponse, final HttpContext httpContext) {

    try {
      final SentiloRequest request = SentiloRequest.build(httpRequest);
      final SentiloResponse response = SentiloResponse.build(httpResponse);
      debug(request);
      request.checkCredentialIntegrity(authenticationService);

      final AbstractHandler handler = lookupHandlerForRequest(request);
      handler.manageRequest(request, response);

      prepareResponse(httpResponse, request.getContentType().toString());
    } catch (final PlatformException e) {
      final int errorCode = (e.getHttpStatus() != 0 ? e.getHttpStatus() : HttpStatus.SC_INTERNAL_SERVER_ERROR);

      prepareErrorResponse(httpResponse, errorCode, e.getMessage(), e.getErrorDetails());
    } catch (final PlatformAccessException e) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.SENTILO_ACCESS_ERROR);
      logger.error("{} - Internal access error.", internalErrorCode, e);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      final int errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR;

      prepareErrorResponse(httpResponse, errorCode, errorMessage);
    } catch (final Throwable e) {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.SENTILO_UNKNOWN_ERROR);
      logger.error("{} - Internal server error.", internalErrorCode, e);
      final String errorMessage = String.format(SentiloConstants.INTERNAL_ERROR_MESSAGE_TEMPLATE, internalErrorCode);
      final int errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR;

      prepareErrorResponse(httpResponse, errorCode, errorMessage);
    }
    debug(httpResponse);
  }

  private void prepareErrorResponse(final HttpResponse response, final int errorCode, final String errorMessage) {
    prepareErrorResponse(response, errorCode, errorMessage, Collections.<String>emptyList());
  }

  private void prepareErrorResponse(final HttpResponse response, final int errorCode, final String errorMessage, final List<String> errorDetails) {
    final ErrorMessage message = new ErrorMessage(errorCode, errorMessage, errorDetails);

    try {
      final ByteArrayOutputStream baos = errorParser.writeInternal(message);
      response.setStatusCode(errorCode);
      response.setEntity(new ByteArrayEntity(baos.toByteArray(), PlatformJsonMessageConverter.DEFAULT_CONTENT_TYPE));
      response.setHeader(HttpHeader.CONTENT_TYPE.toString(), PlatformJsonMessageConverter.DEFAULT_CONTENT_TYPE.toString());
    } catch (final JsonConverterException jce) {
      response.setStatusCode(errorCode);
      response.setEntity(new ByteArrayEntity(jce.getMessage().getBytes()));
    }
  }

  private void prepareResponse(final HttpResponse httpResponse, final String contentType) {
    httpResponse.setStatusCode(HttpStatus.SC_OK);
    httpResponse.setHeader(HttpHeader.CONTENT_TYPE.toString(), contentType);
  }

  private AbstractHandler lookupHandlerForRequest(final SentiloRequest request) throws PlatformException {
    logger.debug("Looking handler for request {}", request.getUri());

    final AbstractHandler handler = handlerLocator.lookup(request);
    if (handler == null) {
      throw new PlatformException(HttpStatus.SC_NOT_FOUND, "handler not found to process request " + request.getUri());
    }
    return handler;
  }

  private void debug(final SentiloRequest request) {
    if (logger.isDebugEnabled()) {
      logger.debug("New http {} request: {}", request.getMethod(), request.getUri());
      logger.debug("Content-Type: {}", request.getContentType());
    }
  }

  private void debug(final HttpResponse httpResponse) {
    if (logger.isDebugEnabled()) {
      logger.debug("New http response:");

      if (httpResponse.getStatusLine() != null) {
        final int status = httpResponse.getStatusLine().getStatusCode();
        logger.debug("Status: {}", status);
      }

      final Header[] header = httpResponse.getHeaders(HttpHeader.CONTENT_TYPE.toString());
      if (header != null && header.length > 0) {
        final String contentType = header[0].getValue();
        logger.debug("{} : {}", HttpHeader.CONTENT_TYPE.toString(), contentType);
      }

      if (httpResponse.getEntity() != null) {
        try {
          logger.debug("Entity body: {} ", EntityUtils.toString(httpResponse.getEntity()));
        } catch (final Exception e) {
          logger.error("Error parsing body", e);
        }
      }
    }
  }
}
