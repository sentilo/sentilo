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
package org.sentilo.platform.client.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.sentilo.platform.client.test.aop.PlatformExceptionTranslationInterceptorTest;
import org.sentilo.platform.client.test.core.PlatformTemplateTest;
import org.sentilo.platform.client.test.domain.ClientDomainTest;
import org.sentilo.platform.client.test.domain.SubscribeInputMessageTest;
import org.sentilo.platform.client.test.exception.SentiloExceptionsTest;
import org.sentilo.platform.client.test.parser.AlarmMessageConverterTest;
import org.sentilo.platform.client.test.parser.CatalogAlertMessageConverterTest;
import org.sentilo.platform.client.test.parser.CatalogMessageConverterTest;
import org.sentilo.platform.client.test.parser.DataMessageConverterTest;
import org.sentilo.platform.client.test.parser.OrderMessageConverterTest;
import org.sentilo.platform.client.test.parser.SubscribeMessageConverterTest;
import org.sentilo.platform.client.test.service.DefaultAlarmServiceOperationsImplTest;
import org.sentilo.platform.client.test.service.DefaultCatalogServiceOperationsImplTest;
import org.sentilo.platform.client.test.service.DefaultDataServiceOperationsImplTest;
import org.sentilo.platform.client.test.service.DefaultOrderServiceOperationsImplTest;
import org.sentilo.platform.client.test.service.DefaultSubscribeServiceOperationsImplTest;
import org.sentilo.platform.client.test.utils.RequestUtilsTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({RequestUtilsTest.class, SubscribeInputMessageTest.class, SubscribeMessageConverterTest.class, AlarmMessageConverterTest.class,
    OrderMessageConverterTest.class, DataMessageConverterTest.class, CatalogMessageConverterTest.class, CatalogAlertMessageConverterTest.class,
    SentiloExceptionsTest.class, PlatformExceptionTranslationInterceptorTest.class, ClientDomainTest.class, PlatformTemplateTest.class,
    DefaultAlarmServiceOperationsImplTest.class, DefaultCatalogServiceOperationsImplTest.class, DefaultDataServiceOperationsImplTest.class,
    DefaultOrderServiceOperationsImplTest.class, DefaultSubscribeServiceOperationsImplTest.class})
public class AllTests {

}
