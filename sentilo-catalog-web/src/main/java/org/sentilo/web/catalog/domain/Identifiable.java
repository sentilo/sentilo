package org.sentilo.web.catalog.domain;

import java.io.Serializable;

/**
 * This interface defines classes whose Object instances can be uniquely identified relative to
 * other Object instances within the same class type hierarchy
 *
 */

public interface Identifiable extends Serializable {

  String getId();
}
