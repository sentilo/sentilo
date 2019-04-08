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
import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

/**
 * Global validator used to validate all beans marked with the annotation @Valid.
 *
 */
public class SentiloAnnotationValidator implements Validator, InitializingBean {

  @Autowired
  private List<Validator> validatorBeans;

  private Collection<Validator> validators = new ArrayList<Validator>();

  public void afterPropertiesSet() throws Exception {
    findAnnotatedValidatorBeans();
  }

  public boolean supports(final Class<?> clazz) {
    for (final Validator validator : validators) {
      if (validator.supports(clazz)) {
        return true;
      }
    }

    return false;
  }

  public void validate(final Object target, final Errors errors) {
    for (final Validator validator : validators) {
      if (validator.supports(target.getClass())) {
        validator.validate(target, errors);
      }
    }
  }

  private void findAnnotatedValidatorBeans() {
    for (final Validator bean : validatorBeans) {
      final SentiloValidator annotation = AnnotationUtils.findAnnotation(bean.getClass(), SentiloValidator.class);
      if (annotation != null) {
        validators.add(bean);
      }
    }
  }

  public void setValidators(final Collection<Validator> validators) {
    this.validators = validators;
  }
}
