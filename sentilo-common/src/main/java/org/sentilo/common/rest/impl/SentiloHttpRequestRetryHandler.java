package org.sentilo.common.rest.impl;

import java.io.IOException;

import org.apache.http.NoHttpResponseException;
import org.apache.http.impl.client.DefaultHttpRequestRetryHandler;
import org.apache.http.protocol.HttpContext;

/**
 * Extends default behavior to retry request also when exception is of type NoHttpResponseException
 * (i.e. error is thrown because request is reusing a persistent connection and target server has
 * drop the connection)
 */
public class SentiloHttpRequestRetryHandler extends DefaultHttpRequestRetryHandler {

  public boolean retryRequest(final IOException exception, final int executionCount, final HttpContext context) {

    if (exception instanceof NoHttpResponseException) {
      // Retry if the server dropped connection on us
      return true;
    } else {
      return super.retryRequest(exception, executionCount, context);
    }
  }
}
