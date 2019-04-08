package org.sentilo.web.catalog.admin.upgrade;

import java.util.Date;

import org.sentilo.web.catalog.admin.upgrade.UpgradeDatabaseHook.ItemState;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public class UpgradeDatabaseItem {

  @Id
  private String id;
  private ItemState state;
  private Date updatedAt;
  private String description;

  public UpgradeDatabaseItem() {
    super();
  }

  public UpgradeDatabaseItem(final String id) {
    this();
    this.id = id;
  }

  public UpgradeDatabaseItem(final String id, final ItemState state, final Date updatedAt) {
    this(id);
    this.state = state;
    this.updatedAt = updatedAt;
  }

  public void changeState(final ItemState newState) {
    state = newState;
    updatedAt = new Date();
  }

  public void changeState(final ItemState newState, final String description) {
    changeState(newState);
    this.description = description;
  }

  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public ItemState getState() {
    return state;
  }

  public void setState(final ItemState state) {
    this.state = state;
  }

  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

}
