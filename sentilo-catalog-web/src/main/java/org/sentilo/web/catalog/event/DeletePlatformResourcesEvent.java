package org.sentilo.web.catalog.event;

import java.util.Collection;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.Sensor;
import org.springframework.context.ApplicationEvent;
import org.springframework.data.mongodb.core.mapping.event.AfterDeleteEvent;

/**
 * {@link ApplicationEvent} which indicates that a catalog catalog resource (sensor, component or
 * provider) has been deleted from internal repository.
 * 
 * This event is similar to {@link AfterDeleteEvent} but allows more fine control over when to throw
 * the event, because there are resources that are deleted in cascade (e.g. provider and sensor
 * childs) and only one event must be created .
 */
public class DeletePlatformResourcesEvent<T extends CatalogDocument> extends ApplicationEvent {

  private static final long serialVersionUID = 1L;

  /** Catalog resources that has been deleted from repository */
  private Collection<T> resources;
  private Class<T> type;

  public DeletePlatformResourcesEvent(Object source, Collection<T> resources, final Class<T> type) {
    super(source);
    this.resources = resources;
    this.type = type;
  }

  public Collection<T> getResources() {
    return resources;
  }

  public boolean resourcesAreSensors(){
    return type.isAssignableFrom(Sensor.class);
  }

}
