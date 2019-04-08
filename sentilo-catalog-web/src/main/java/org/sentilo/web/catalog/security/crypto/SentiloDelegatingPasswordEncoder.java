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
