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
