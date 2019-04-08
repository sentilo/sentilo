package org.sentilo.common.domain;

/**
 * Interface which identifies a mutable catalog element, i.e., an object that can change its state
 * after it has been created.
 */
public interface MutableCatalogElement extends CatalogElement {

  Long getUpdatedAt();
}
