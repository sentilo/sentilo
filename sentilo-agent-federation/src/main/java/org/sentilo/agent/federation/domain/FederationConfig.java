package org.sentilo.agent.federation.domain;

import java.io.Serializable;
import java.util.Date;

import org.hibernate.validator.constraints.NotBlank;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;

@Document
public class FederationConfig implements Serializable {

  private static final long serialVersionUID = 1L;

  @Id
  private String id;

  @NotBlank
  private String name;

  private boolean active;

  private String description;
  private String appClientName;
  private String appClientToken;
  private String sourceEndpoint;
  private String sourceContactName;
  private String sourceContactMail;

  @DateTimeFormat(pattern = "dd/MM/yyyy HH:mm Z")
  private Date createdAt;

  @DateTimeFormat(pattern = "dd/MM/yyyy HH:mm Z")
  private Date updatedAt;

  private String tenantId;

  public FederationConfig() {
  }

  public String getId() {
    return id;
  }

  public void setId(final String id) {
    this.id = id;
  }

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(final String description) {
    this.description = description;
  }

  public String getAppClientName() {
    return appClientName;
  }

  public void setAppClientName(final String appClientName) {
    this.appClientName = appClientName;
  }

  public String getAppClientToken() {
    return appClientToken;
  }

  public void setAppClientToken(final String appClientToken) {
    this.appClientToken = appClientToken;
  }

  public String getSourceEndpoint() {
    return sourceEndpoint;
  }

  public void setSourceEndpoint(final String sourceEndpoint) {
    this.sourceEndpoint = sourceEndpoint;
  }

  public String getSourceContactName() {
    return sourceContactName;
  }

  public void setSourceContactName(final String sourceContactName) {
    this.sourceContactName = sourceContactName;
  }

  public String getSourceContactMail() {
    return sourceContactMail;
  }

  public void setSourceContactMail(final String sourceContactMail) {
    this.sourceContactMail = sourceContactMail;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  public Date getUpdatedAt() {
    return updatedAt;
  }

  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  public String getTenantId() {
    return tenantId;
  }

  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(final boolean active) {
    this.active = active;
  }

}
