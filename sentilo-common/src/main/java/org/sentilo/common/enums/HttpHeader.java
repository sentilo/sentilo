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
package org.sentilo.common.enums;

public enum HttpHeader {

  CONTENT_TYPE("Content-Type"), IDENTITY_KEY("IDENTITY_KEY"), X_FORWARDED_PROTO("X-Forwarded-Proto"), X_FORWARDED_FOR("X-Forwarded-For"), X_RL_GLOBAL_INPUT_LIMIT("X-RateLimit-Global-Inbound-Limit"), X_RL_GLOBAL_INPUT_REMAINING("X-RateLimit-Global-Inbound-Remaining"), X_RL_GLOBAL_INPUT_RESET("X-RateLimit-Global-Inbound-Reset"), X_RL_INPUT_LIMIT("X-RateLimit-Inbound-Limit"), X_RL_INPUT_REMAINING("X-RateLimit-Inbound-Remaining"), X_RL_INPUT_RESET("X-RateLimit-Inbound-Reset"), X_RL_OUTPUT_LIMIT("X-RateLimit-Outbound-Limit"), X_RL_OUTPUT_REMAINING("X-RateLimit-Outbound-Remaining"), X_RL_OUTPUT_RESET("X-RateLimit-Outbound-Reset");

  private String name;

  private HttpHeader(final String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return name;
  }
}
