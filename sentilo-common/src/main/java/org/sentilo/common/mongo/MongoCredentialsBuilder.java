package org.sentilo.common.mongo;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

public class MongoCredentialsBuilder {

  public static String encodeCredentials(final String username, final String password, final String database) {
    // MongoCredentials match the pattern username:password@database where username and password
    // must be url-encoded
    // https://docs.spring.io/spring-data/mongodb/docs/2.0.9.RELEASE/reference/html/
    final String credentialsExp = "%s:%s@%s";
    return String.format(credentialsExp, encodeParameter(username), encodeParameter(password), database);
  }

  private static String encodeParameter(final String value) {
    try {
      return URLEncoder.encode(value, "UTF-8");
    } catch (final UnsupportedEncodingException e) {
      throw new IllegalArgumentException("o_O UTF-8 not supported!", e);
    }
  }

}
