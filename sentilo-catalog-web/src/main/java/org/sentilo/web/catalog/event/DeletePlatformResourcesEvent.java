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
package org.sentilo.web.catalog.event;

import java.util.Collection;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Sensor;
import org.springframework.context.ApplicationEvent;
import org.springframework.data.mongodb.core.mapping.event.AfterDeleteEvent;

/**
 * {@link ApplicationEvent} which indicates that a catalog resource (sensor, component or provider)
 * has been deleted from internal repository.
 * 
 * This event is similar to {@link AfterDeleteEvent} but allows more fine control over when to throw
 * the event, because there are resources that are deleted in cascade (e.g. provider and sensor
 * childs) and only one event must be created .
 */
public class DeletePlatformResourcesEvent<T extends CatalogDocument> extends ApplicationEvent {

  private static final long serialVersionUID = 1L;

  /** Catalog resources that has been deleted from repository */
  private final Collection<T> resources;
  private final Class<T> type;

  public DeletePlatformResourcesEvent(final Object source, final Collection<T> resources, final Class<T> type) {
    super(source);
    this.resources = resources;
    this.type = type;
  }

  public Collection<T> getResources() {
    return resources;
  }

  public boolean resourcesAreSensors() {
    return type.isAssignableFrom(Sensor.class);
  }

}
