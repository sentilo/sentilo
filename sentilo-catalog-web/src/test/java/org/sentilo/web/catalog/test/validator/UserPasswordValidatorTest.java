package org.sentilo.web.catalog.test.validator;

import static org.mockito.Mockito.when;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.dto.ChangeUserPasswordDTO;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.validator.UserPasswordValidator;
import org.springframework.context.MessageSource;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;

public class UserPasswordValidatorTest {

  @Mock
  private MessageSource messageSource;

  @Mock
  private CatalogUserDetails userDetails;

  @Mock
  private SecurityContext securityContext;

  @Mock
  private Authentication authentication;

  @Mock
  private PasswordEncoder passwordEncoder;

  @InjectMocks
  private UserPasswordValidator validator;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    SecurityContextHolder.clearContext();

    SecurityContextHolder.setContext(securityContext);
    when(securityContext.getAuthentication()).thenReturn(authentication);
    when(authentication.getPrincipal()).thenReturn(userDetails);
  }

  @After
  public void tearDown() {
    SecurityContextHolder.clearContext();
  }

  @Test
  public void changePasswordOk() {

    final String currentPwd = "12345";
    final String newPwd = "1Ab#567898";
    final String repeatNewPwd = "1Ab#567898";

    final ChangeUserPasswordDTO resource = new ChangeUserPasswordDTO();
    resource.setCurrentPassword(currentPwd);
    resource.setNewPassword(newPwd);
    resource.setPasswordRepeat(repeatNewPwd);

    when(userDetails.getPassword()).thenReturn(currentPwd);
    when(passwordEncoder.matches(userDetails.getPassword(), currentPwd)).thenReturn(true);

    final Errors errors = new BeanPropertyBindingResult(resource, "changePassword");

    validator.validate(resource, errors);

    Assert.assertTrue(!errors.hasErrors());
  }

  @Test
  public void currentPasswordNotMatch() {
    final String currentPwd = "12345";
    final String newPwd = "1Ab#56789";
    final String repeatNewPwd = "1Ab#567898";

    final ChangeUserPasswordDTO resource = new ChangeUserPasswordDTO();
    resource.setCurrentPassword(currentPwd + "_");
    resource.setNewPassword(newPwd);
    resource.setPasswordRepeat(repeatNewPwd);

    when(userDetails.getPassword()).thenReturn(currentPwd);
    when(passwordEncoder.matches(userDetails.getPassword(), currentPwd)).thenReturn(false);

    final Errors errors = new BeanPropertyBindingResult(resource, "changePassword");

    validator.validate(resource, errors);

    Assert.assertTrue(errors.hasErrors());
    Assert.assertTrue(errors.hasFieldErrors("currentPassword"));
  }

  @Test
  public void newAndRepeatPasswordNotMatch() {
    final String currentPwd = "12345";
    final String newPwd = "1Ab#56789";
    final String repeatNewPwd = "1Ab#567898";

    final ChangeUserPasswordDTO resource = new ChangeUserPasswordDTO();
    resource.setCurrentPassword(currentPwd);
    resource.setNewPassword(newPwd);
    resource.setPasswordRepeat(repeatNewPwd);

    when(userDetails.getPassword()).thenReturn(currentPwd);
    when(passwordEncoder.matches(userDetails.getPassword(), currentPwd)).thenReturn(true);

    final Errors errors = new BeanPropertyBindingResult(resource, "changePassword");

    validator.validate(resource, errors);

    Assert.assertTrue(errors.hasErrors());
    Assert.assertTrue(errors.hasFieldErrors("passwordRepeat"));
  }

  @Test
  public void wrongPasswordStrength() {
    final String currentPwd = "12345";
    final String newPwd = "1b#56789";
    final String repeatNewPwd = "1b#56789";

    final ChangeUserPasswordDTO resource = new ChangeUserPasswordDTO();
    resource.setCurrentPassword(currentPwd);
    resource.setNewPassword(newPwd);
    resource.setPasswordRepeat(repeatNewPwd);

    when(userDetails.getPassword()).thenReturn(currentPwd);
    when(passwordEncoder.matches(userDetails.getPassword(), currentPwd)).thenReturn(true);

    final Errors errors = new BeanPropertyBindingResult(resource, "changePassword");

    validator.validate(resource, errors);

    Assert.assertTrue(errors.hasErrors());
    Assert.assertTrue(errors.hasFieldErrors("newPassword"));
  }

}
