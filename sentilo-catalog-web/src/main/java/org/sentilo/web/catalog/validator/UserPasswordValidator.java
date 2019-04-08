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
