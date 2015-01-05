/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
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
package org.sentilo.web.catalog.aop.aspect;

import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.sentilo.common.utils.SentiloConstants;
import org.sentilo.common.utils.SentiloUtils;
import org.sentilo.web.catalog.exception.CatalogException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.Ordered;
import org.springframework.stereotype.Component;

/**
 * AOP aspect that intercepts every exception thrown from a service method and translates it to a
 * CatalogException if needed.
 */
@Component
@Aspect
public class CatalogExceptionTranslationInterceptor implements Ordered {

  private final Logger logger = LoggerFactory.getLogger(CatalogExceptionTranslationInterceptor.class);

  private final int order = 1;

  private final String internalSystemErrorKey = "error.internal.key";

  @Pointcut("execution(* org.sentilo.web.catalog.service..*.*(..))")
  public void anyTemplateMethodPointcut() {
  }

  @AfterThrowing(pointcut = "anyTemplateMethodPointcut())", throwing = "ex")
  public void doTranslationAction(final RuntimeException ex) {
    if (ex instanceof CatalogException) {
      throw ex;
    } else {
      final String internalErrorCode = SentiloUtils.buildNewInternalErrorCode(SentiloConstants.CATALOG_GENERAL_ERROR);
      logger.error("{} - Internal system error.", internalErrorCode, ex);

      throw new CatalogException(internalSystemErrorKey, new Object[] {internalErrorCode}, ex);
    }
  }

  @Override
  public int getOrder() {
    return order;
  }
}
