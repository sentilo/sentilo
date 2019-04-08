package org.sentilo.common.test.mongo;

import org.junit.Assert;
import org.junit.Test;
import org.sentilo.common.mongo.MongoCredentialsBuilder;

public class MongoCredentialsBuilderTest {

  @Test
  public void encodeCredentials() {
    final String user = "sentilo";
    final String password = "secret-key";
    final String dbName = "sentiloDb";

    final String credentialsDb = MongoCredentialsBuilder.encodeCredentials(user, password, dbName);

    Assert.assertEquals("sentilo:secret-key@sentiloDb", credentialsDb);
  }

  @Test
  public void encodeCredentialsWithRestrictedCharacters() {
    final String user = "sent:ilo";
    final String password = "secret%key@01";
    final String dbName = "sentiloDb";

    final String credentialsDb = MongoCredentialsBuilder.encodeCredentials(user, password, dbName);

    Assert.assertEquals("sent%3Ailo:secret%25key%4001@sentiloDb", credentialsDb);
  }

}
