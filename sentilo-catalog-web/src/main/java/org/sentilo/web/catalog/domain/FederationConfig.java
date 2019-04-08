package org.sentilo.web.catalog.domain;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;

import org.hibernate.validator.constraints.URL;
import org.sentilo.web.catalog.utils.Constants;
import org.springframework.format.annotation.DateTimeFormat;

public class FederationConfig implements CatalogDocument, TenantResource, AlphabeticalSortable {

  private static final long serialVersionUID = 1L;

  @NotBlank
  @Pattern(regexp = Constants.VALIDATION_FEDERATION_ID_REGEXP, message = "{sentilo.valid.federation.id}")
  private String id;

  @NotBlank
  private String name;

  @NotBlank
  private String description;

  @NotBlank
  private String appClientName;

  @NotBlank
  private String appClientToken;

  @NotBlank
  @URL
  private String sourceEndpoint;

  @NotBlank
  private String sourceContactName;

  @NotBlank
  @Email
  private String sourceContactMail;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date lastSyncTime;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date createdAt;

  private String createdBy;

  @DateTimeFormat(pattern = Constants.DATETIME_FORMAT)
  private Date updatedAt;

  private String updatedBy;

  private String tenantId;

  private Set<String> tenantsAuth;

  private Set<String> tenantsListVisible;

  private boolean active;

  public FederationConfig() {
    super();
    tenantsAuth = new HashSet<String>();
    tenantsListVisible = new HashSet<String>();
  }

  public FederationConfig(final String id) {
    this();
    this.id = id;
  }

  @Override
  public boolean equals(final Object obj) {
    if (!(obj instanceof FederationConfig)) {
      return false;
    }

    if (id == null) {
      return false;
    }

    final FederationConfig other = (FederationConfig) obj;
    return id.equals(other.getId());
  }

  @Override
  public int hashCode() {
    // Hashcode return must be consistent with the equals method
    final int prime = 37;
    int result = 1;
    result = prime * result + (id == null ? 0 : id.hashCode());
    return result;
  }

  @Override
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

  @Override
  public void setUpdatedAt(final Date updatedAt) {
    this.updatedAt = updatedAt;
  }

  @Override
  public Date getUpdatedAt() {
    return updatedAt;
  }

  @Override
  public void setCreatedAt(final Date createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  public Date getCreatedAt() {
    return createdAt;
  }

  @Override
  public String getCreatedBy() {
    return createdBy;
  }

  @Override
  public void setCreatedBy(final String createdBy) {
    this.createdBy = createdBy;
  }

  @Override
  public String getUpdatedBy() {
    return updatedBy;
  }

  @Override
  public void setUpdatedBy(final String updatedBy) {
    this.updatedBy = updatedBy;
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

  public Date getLastSyncTime() {
    return lastSyncTime;
  }

  public void setLastSyncTime(final Date lastSyncTime) {
    this.lastSyncTime = lastSyncTime;
  }

  @Override
  public String getSortableValue() {
    return name;
  }

  @Override
  public String getTenantId() {
    return tenantId;
  }

  @Override
  public void setTenantId(final String tenantId) {
    this.tenantId = tenantId;
  }

  @Override
  public Set<String> getTenantsAuth() {
    return tenantsAuth;
  }

  @Override
  public void setTenantsAuth(final Set<String> tenantsAuth) {
    this.tenantsAuth = tenantsAuth;
  }

  @Override
  public Set<String> getTenantsListVisible() {
    return tenantsListVisible;
  }

  @Override
  public void setTenantsListVisible(final Set<String> tenantsListVisible) {
    this.tenantsListVisible = tenantsListVisible;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(final boolean active) {
    this.active = active;
  }

}
