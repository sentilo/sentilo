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
package org.sentilo.platform.common.exception;

import java.util.Set;

import org.apache.http.HttpStatus;
import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.SentiloConstants;
import org.springframework.util.StringUtils;

/**
 * Exception thrown when a request to put data cannot be accepted (partially or completely) due to
 * either the target resource does not exist or it is not enabled.
 */
public class EventRejectedException extends PlatformException {

  private static final long serialVersionUID = 1L;

  private static final String MSG_TEMPLATE =
      "Events associated with resources [%s] have been rejected. See the error details to know about what went wrong in each case";

  private final EventType eventType;

  private final int rejectedEventsCount;

  private Set<String> rejectedResources;

  public EventRejectedException(final EventType eventType, final RejectedResourcesContext rejectedContext) {
    super(HttpStatus.SC_NOT_FOUND, String.format(MSG_TEMPLATE, StringUtils.collectionToCommaDelimitedString(rejectedContext.getRejectedResources())),
        rejectedContext.getRejectedErrorDetails());
    this.eventType = eventType;
    rejectedResources = rejectedContext.getRejectedResources();
    rejectedEventsCount = rejectedContext.getRejectedEventsCount();
    setInternalErrorCode(SentiloConstants.PUBSUB_EVENT_REJECTED);
  }

  public EventType getEventType() {
    return eventType;
  }

  public int getRejectedEventsCount() {
    return rejectedEventsCount;
  }

  public Set<String> getRejectedResources() {
    return rejectedResources;
  }

}
