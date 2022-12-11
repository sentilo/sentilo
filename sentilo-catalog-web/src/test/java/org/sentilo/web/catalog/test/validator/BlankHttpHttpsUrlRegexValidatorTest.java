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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import javax.validation.Validation;

import org.hibernate.validator.constraints.URL;
import org.junit.Before;
import org.junit.Test;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.SpringValidatorAdapter;

public class BlankHttpHttpsUrlRegexValidatorTest {

  private Validator validator;

  @Before
  public void createValidator() {
    validator = new SpringValidatorAdapter(Validation.buildDefaultValidatorFactory().getValidator());
  }

  private Errors invokeValidator(final Object bean) {
    final BeanPropertyBindingResult errors = new BeanPropertyBindingResult(bean, "bean");
    ValidationUtils.invokeValidator(validator, bean, errors);
    return errors;
  }

  @Test
  public void checkEmpty() {
    final TestBean bean = new TestBean();
    bean.myUrl = "";
    final Errors actual = invokeValidator(bean);
    assertFalse(actual.hasErrors());
  }

  @Test
  public void checkNull() {
    final TestBean bean = new TestBean();
    bean.myUrl = null;
    final Errors actual = invokeValidator(bean);
    assertFalse(actual.hasErrors());
  }

  @Test
  public void checkHttp() {
    final TestBean bean = new TestBean();
    bean.myUrl = "http://www.google.es";
    final Errors actual = invokeValidator(bean);
    assertFalse(actual.hasErrors());
  }

  @Test
  public void checkHttps() {
    final TestBean bean = new TestBean();
    bean.myUrl = "https://www.google.es";
    final Errors actual = invokeValidator(bean);
    assertFalse(actual.hasErrors());
  }

  @Test
  public void checkFtpNotAccepted() {
    final TestBean bean = new TestBean();
    bean.myUrl = "ftp://my.host";
    final Errors actual = invokeValidator(bean);
    assertTrue(actual.hasErrors());
  }

  @Test
  public void checkWsNotAccepted() {
    final TestBean bean = new TestBean();
    bean.myUrl = "ws://my.host";
    final Errors actual = invokeValidator(bean);
    assertTrue(actual.hasErrors());
  }

  @Test
  public void checkNonURLNotAccepted() {
    final TestBean bean = new TestBean();
    bean.myUrl = "some string here";
    final Errors actual = invokeValidator(bean);
    assertTrue(actual.hasErrors());
  }

  private static class TestBean {

    @URL(regexp = Constants.VALIDATION_EMPTY_HTTP_HTTPS_URL)
    private String myUrl;
  }

}
