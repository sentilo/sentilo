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
package org.sentilo.web.catalog.test.utils;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.utils.LastUpdateMessageBuilder;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;

public class LastUpdateMessageBuilderTest {

  @Mock
  private MessageSource messageSource;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void buildUnknownMessage() {
    LastUpdateMessageBuilder.buildMessage(messageSource, 0);

    verify(messageSource).getMessage("component.lastupdated.time.unknown", new Object[] {0l}, LocaleContextHolder.getLocale());
  }

  @Test
  public void buildDayMessage() {
    final long currentTime = System.currentTimeMillis();
    final long lastUpdateTime = currentTime - 3 * 24 * 60 * 60 * 1000;

    LastUpdateMessageBuilder.buildMessage(messageSource, lastUpdateTime);

    verify(messageSource).getMessage(eq("component.lastupdated.time.days"), new Object[] {any(Long.class)}, eq(LocaleContextHolder.getLocale()));
  }

  @Test
  public void buildHoursMessage() {
    final long currentTime = System.currentTimeMillis();
    final long lastUpdateTime = currentTime - 2 * 60 * 60 * 1000;

    LastUpdateMessageBuilder.buildMessage(messageSource, lastUpdateTime);

    verify(messageSource).getMessage(eq("component.lastupdated.time.hours"), new Object[] {any(Long.class)}, eq(LocaleContextHolder.getLocale()));
  }

  @Test
  public void buildMinutesMessage() {
    final long currentTime = System.currentTimeMillis();
    final long lastUpdateTime = currentTime - 31 * 60 * 1000;
    LastUpdateMessageBuilder.buildMessage(messageSource, lastUpdateTime);

    verify(messageSource).getMessage(eq("component.lastupdated.time.minutes"), new Object[] {any(Long.class)}, eq(LocaleContextHolder.getLocale()));
  }

  @Test
  public void buildSecondsMessage() {
    final long currentTime = System.currentTimeMillis();
    final long lastUpdateTime = currentTime - 4 * 1000;

    LastUpdateMessageBuilder.buildMessage(messageSource, lastUpdateTime);

    verify(messageSource).getMessage(eq("component.lastupdated.time.seconds"), new Object[] {any(Long.class)}, eq(LocaleContextHolder.getLocale()));
  }
}
