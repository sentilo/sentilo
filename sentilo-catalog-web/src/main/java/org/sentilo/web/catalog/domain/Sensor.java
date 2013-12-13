/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.domain;

import java.util.Date;
import java.util.List;

import javax.validation.constraints.Pattern;

import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.hibernate.validator.constraints.NotBlank;
import org.sentilo.web.catalog.utils.CompoundKeyBuilder;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TagUtils;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.StringUtils;


@Document
public class Sensor implements CatalogDocument {

	private static final long serialVersionUID = 1L;

	public enum DataType {
		NUMBER, BOOLEAN, TEXT,
	}

	@Id
	private String id;

	@NotBlank
	@Pattern(regexp = Constants.VALIDATION_ENTITY_NAME_REGEXP)
	private String sensorId;

	@NotBlank
	private String providerId;
	
	@NotBlank
	private String componentId;

	private String description;
	private DataType dataType;

	@DateTimeFormat(pattern = Constants.DATE_FORMAT)
	private Date createdAt;

	@DateTimeFormat(pattern = Constants.DATE_FORMAT)
	private Date updateAt;

	@NotBlank
	private String type;

	@NotBlank
	private String unit;
	private String validTime;
	
	//Additional info
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String metaData;

	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String manufacturer;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String modelReference;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String serialNumber;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	@DateTimeFormat(pattern = Constants.DATE_FORMAT)
	private Date installationDate;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String parentId;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String ipAddress;
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String observations;	
	@JsonSerialize(include=Inclusion.NON_EMPTY)
	private String tags;

	private Boolean publicAccess = Boolean.FALSE;

	public Sensor() {

	}

	public Sensor(String id) {
		this();
		this.id = id;
	}
	
	
	public Sensor(String providerId, String componentId, String sensorId){
		this();
		this.providerId = providerId;
		this.sensorId = sensorId;
		this.componentId = componentId;
		this.id = getId();
	}

	
	public static String buildId(String componentId, String sensorId){				
		return CompoundKeyBuilder.buildCompoundKey(componentId,sensorId);							
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		return result*super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Sensor other = (Sensor) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}
	
	

	public String getId() {
		if (!StringUtils.hasText(this.id) && StringUtils.hasText(sensorId) && StringUtils.hasText(componentId)) {
			this.id = buildId(componentId, sensorId);
		}

		return this.id;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public DataType getDataType() {
		return dataType;
	}

	public void setDataType(DataType dataType) {
		this.dataType = dataType;
	}

	public String getUnit() {
		return unit;
	}

	public void setUnit(String unit) {
		this.unit = unit;
	}

	public String getValidTime() {
		return validTime;
	}

	public void setValidTime(String validTime) {
		this.validTime = validTime;
	}

	public String getMetaData() {
		return metaData;
	}

	public void setMetaData(String metaData) {
		this.metaData = metaData;
	}

	public String getProviderId() {
		return providerId;
	}

	public void setProviderId(String providerId) {
		this.providerId = providerId;
	}

	public void setSensorId(String sensorId) {
		this.sensorId = sensorId;
	}

	public String getSensorId() {
		return sensorId;
	}

	public void setCreatedAt(Date createdAt) {
		this.createdAt = createdAt;
	}

	public Date getCreatedAt() {
		return createdAt;
	}

	public void setUpdateAt(Date updateAt) {
		this.updateAt = updateAt;
	}

	public Date getUpdateAt() {
		return updateAt;
	}

	public String getManufacturer() {
		return manufacturer;
	}

	public void setManufacturer(String manufacturer) {
		this.manufacturer = manufacturer;
	}

	public String getModelReference() {
		return modelReference;
	}

	public void setModelReference(String modelReference) {
		this.modelReference = modelReference;
	}

	public String getSerialNumber() {
		return serialNumber;
	}

	public void setSerialNumber(String serialNumber) {
		this.serialNumber = serialNumber;
	}

	public Date getInstallationDate() {
		return installationDate;
	}

	public void setInstallationDate(Date installationDate) {
		this.installationDate = installationDate;
	}

	public String getParentId() {
		return parentId;
	}

	public void setParentId(String parentId) {
		this.parentId = parentId;
	}

	public String getIpAddress() {
		return ipAddress;
	}

	public void setIpAddress(String ipAddress) {
		this.ipAddress = ipAddress;
	}

	public String getObservations() {
		return observations;
	}

	public void setObservations(String observations) {
		this.observations = observations;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getTags() {
		return tags;
	}

	public List<String> getTagsAsList() {
		return TagUtils.toStringList(tags);
	}

	public void setTags(String tags) {
		this.tags = tags;
	}

	public Boolean getPublicAccess() {
		return publicAccess;
	}

	public void setPublicAccess(Boolean publicAccess) {
		this.publicAccess = publicAccess;
	}

	public String getComponentId() {
		return componentId;
	}

	public void setComponentId(String componentId) {
		this.componentId = componentId;
	}
}
