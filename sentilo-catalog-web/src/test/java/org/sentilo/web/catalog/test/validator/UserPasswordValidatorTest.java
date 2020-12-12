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
