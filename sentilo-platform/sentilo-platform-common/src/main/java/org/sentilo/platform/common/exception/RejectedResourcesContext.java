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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Helper class to encapsulate and build the attributes of an {@link EventRejectedException}
 *
 */
public class RejectedResourcesContext {

  final Set<String> rejectedResources = new HashSet<String>();
  final List<String> rejectedErrorDetails = new ArrayList<String>();
  int rejectedEventsCount = 0;

  public RejectedResourcesContext() {

  }

  public void rejectEvent(final String targetResource, final String errorDetail) {
    final boolean added = rejectedResources.add(targetResource);
    // If rejectedResources did not already contain targetResource then errorDetail is added to the
    // rejectedErrorDetails (it does not contain duplicates)
    if (added) {
      rejectedErrorDetails.add(errorDetail);
    }

    rejectedEventsCount++;
  }

  public boolean isEmpty() {
    return rejectedResources.isEmpty();
  }

  public Set<String> getRejectedResources() {
    return rejectedResources;
  }

  public List<String> getRejectedErrorDetails() {
    return rejectedErrorDetails;
  }

  public int getRejectedEventsCount() {
    return rejectedEventsCount;
  }

}
