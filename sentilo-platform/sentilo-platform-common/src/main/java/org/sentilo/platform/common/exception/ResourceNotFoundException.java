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

import org.apache.http.HttpStatus;
import org.sentilo.common.utils.SentiloConstants;

/**
 * Exception thrown when a request is rejected because applies over a resource that does not exist
 * in Redis.
 */
public class ResourceNotFoundException extends PlatformException {

  private static final long serialVersionUID = 1L;

  private static final String MSG_TEMPLATE = "%s [%s] not found on Sentilo (%s).";

  private final String resourceId;
  private final String resourceType;

  public ResourceNotFoundException(final String resourceId, final String resourceType) {
    super(HttpStatus.SC_NOT_FOUND, String.format(MSG_TEMPLATE, resourceType, resourceId, SentiloConstants.PUBSUB_RESOURCE_NOT_FOUND));
    setInternalErrorCode(SentiloConstants.PUBSUB_RESOURCE_NOT_FOUND);
    this.resourceId = resourceId;
    this.resourceType = resourceType;
  }

  public String getResourceId() {
    return resourceId;
  }

  public String getResourceType() {
    return resourceType;
  }
}
