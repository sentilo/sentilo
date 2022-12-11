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
package org.sentilo.web.catalog.validator;

import java.util.ArrayList;
import java.util.List;

import org.passay.CharacterCharacteristicsRule;
import org.passay.CharacterRule;
import org.passay.EnglishCharacterData;
import org.passay.LengthRule;
import org.passay.PasswordData;
import org.passay.PasswordValidator;
import org.passay.Rule;
import org.passay.RuleResult;
import org.passay.WhitespaceRule;
import org.sentilo.web.catalog.dto.ChangeUserPasswordDTO;
import org.sentilo.web.catalog.security.CatalogUserDetails;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@SentiloValidator
public class UserPasswordValidator implements Validator {

  @Autowired
  private MessageSource messageSource;

  @Autowired
  private PasswordEncoder passwordEncoder;

  @Override
  public boolean supports(final Class<?> aClass) {
    return ChangeUserPasswordDTO.class.isAssignableFrom(aClass);
  }

  @Override
  public void validate(final Object o, final Errors errors) {

    final ChangeUserPasswordDTO resource = (ChangeUserPasswordDTO) o;
    final CatalogUserDetails catalogUser = (CatalogUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    final String oldPassword = catalogUser.getPassword();

    if (!(catalogUser.isAdminUser() || catalogUser.isSuperAdminUser())) {
      if (!StringUtils.hasText(resource.getCurrentPassword()) || !passwordEncoder.matches(resource.getCurrentPassword(), oldPassword)) {
        errors.rejectValue("currentPassword", messageSource.getMessage("user.password.incorrect", new Object[] {}, LocaleContextHolder.getLocale()));
      }
    }

    if (!StringUtils.hasText(resource.getNewPassword())) {
      errors.rejectValue("newPassword", Constants.NOT_BLANK_ERROR);
    } else if (!resource.getNewPassword().equals(resource.getPasswordRepeat())) {
      errors.rejectValue("passwordRepeat", messageSource.getMessage("user.password.notmatch", new Object[] {}, LocaleContextHolder.getLocale()));
    }

    // Finally, if there isn't previous errors, password strength is validated
    if (!errors.hasErrors() && !checkPasswordStrength(resource)) {
      errors.rejectValue("newPassword", messageSource.getMessage("user.password.strength", new Object[] {}, LocaleContextHolder.getLocale()));
    }
  }

  private boolean checkPasswordStrength(final ChangeUserPasswordDTO resource) {
    final List<Rule> rules = new ArrayList<>();

    final CharacterCharacteristicsRule charRule =
        new CharacterCharacteristicsRule(4, new CharacterRule(EnglishCharacterData.Digit, 1), new CharacterRule(EnglishCharacterData.Special, 1),
            new CharacterRule(EnglishCharacterData.UpperCase, 1), new CharacterRule(EnglishCharacterData.LowerCase, 1));

    final WhitespaceRule whitespaceRule = new WhitespaceRule();

    final LengthRule lengthRule = new LengthRule(8, 16);

    rules.add(charRule);
    rules.add(whitespaceRule);
    rules.add(lengthRule);

    final PasswordValidator validator = new PasswordValidator(rules);
    final RuleResult result = validator.validate(new PasswordData(resource.getNewPassword()));
    return result.isValid();
  }

}
