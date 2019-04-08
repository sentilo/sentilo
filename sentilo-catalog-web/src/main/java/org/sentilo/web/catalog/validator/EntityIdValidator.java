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

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.PatternSyntaxException;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import org.hibernate.validator.internal.constraintvalidators.bv.PatternValidator;
import org.sentilo.web.catalog.context.TenantContextHolder;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.util.StringUtils;

/**
 *
 * @see PatternValidator
 */
public class EntityIdValidator implements ConstraintValidator<ValidEntityId, String> {

  private java.util.regex.Pattern pattern;

  @Override
  public void initialize(final ValidEntityId constraintAnnotation) {
    try {
      pattern = java.util.regex.Pattern.compile(constraintAnnotation.regexp());
    } catch (final PatternSyntaxException e) {
      throw getInvalidRegularExpressionException(e, constraintAnnotation.regexp());
    }
  }

  @Override
  public boolean isValid(final String entityId, final ConstraintValidatorContext context) {
    boolean valid = true;
    if (StringUtils.hasText(entityId)) {
      // If Sentilo instance is multitenant, entity id's should follows the expression:
      // tenantId@resourceId
      String resourceId = entityId;
      if (TenantContextHolder.isEnabled()) {
        final String[] tokens = entityId.split(Constants.MULTITENANT_ENTITY_ID_PREPEND_TOKEN);
        resourceId = tokens.length == 2 ? tokens[1] : entityId;
      }
      final Matcher m = pattern.matcher(resourceId);
      valid = m.matches();
    }

    return valid;
  }

  public final IllegalArgumentException getInvalidRegularExpressionException(final PatternSyntaxException e, final String regExp) {
    final IllegalArgumentException result = new IllegalArgumentException(String.format("Invalid regular expression: %s", regExp), e);
    final StackTraceElement[] st = result.getStackTrace();
    result.setStackTrace(Arrays.copyOfRange(st, 1, st.length));
    return result;
  }

}
